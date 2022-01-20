-module(http_cache).

%% API exports
-export([get/2, notify_use/2, cache/3, invalidate_url/2, invalidate_by_alternate_key/2]).

-export_type([alternate_key/0, headers/0, request_key/0, response/0, status/0,
              timestamp/0, vary_headers/0]).

-include_lib("cowlib/include/cow_inline.hrl").
-include_lib("kernel/include/file.hrl").

-type method() :: binary().
-type url() :: binary().
-type headers() :: [{binary(), binary()}].
%% A list of headers. A header can be present multiple times.
-type body() :: iodata().
-type status() :: pos_integer().
-type request() :: {method(), url(), headers(), body()}.
-type response_ref() :: http_cache_store:response_ref().
%% Opaque backend's reference to a response, returned by
%% {@link get/2} and used as a parameter by {@link notify_use/2}.
-type response() :: {status(), headers(), body() | sendfile() | undefined}.
%% HTTP response. The atom `undefined' is returned as a body in case response has
%% to be revalidated (and therefore the body cannot and shouldn't be used).
-type sendfile() ::
    {senfile,
     Offset :: non_neg_integer(),
     Length :: non_neg_integer() | all,
     Path :: binary()}.
-type vary_headers() :: #{binary() := binary() | undefined}.
%% Merged headers on which the response varies. Used by backends.
-type request_key() :: term().
-type alternate_key() :: term().
%% Alternate key attached to a stored response. Used to invalidate by
%% alternate key instead of URL (e.g. to invalidate all the images if
%% the `image' alternate key if set to all images).
-type timestamp() :: non_neg_integer().
%% UNIX timestamp in seconds
-type type() :: shared | private.
-type opts() :: [opt()].
%% Options passed to the functions of this module:
%% <ul>
%%  <li>
%%    `alternate_keys': alternate keys associated with the stored request. Requests
%%    can then be invalidated by alternate key with {@link invalidate_by_alternate_key/2}.
%%    Use by {@link cache/3}.
%%  </li>
%%  <li>
%%    `allow_stale_while_revalidate': allows returning valid stale response while revalidating.
%%    Used by {@link get/2}. Defaults to `false'.
%%  </li>
%%  <li>
%%    `allow_stale_if_error': allows returning valid stale response when an error occurs.
%%    See [https://datatracker.ietf.org/doc/html/rfc5861#section-3].
%%    Used by {@link get/2}. Defaults to `false'.
%%  </li>
%%  <li>
%%    `auto_accept_encoding': automatically selects an acceptable response based on
%%    `accept-encoding' and `content-encoding' headers.
%%
%%    Compressed response vary on the exact value `accept-encoding' header. For example,
%%    `gzip, brotli', `brotli, gzip', `brotli,gzip' and `gzip;q=1.0, brotli;q=1.0' are
%%    equivalent but considered different because their string representation do not match.
%%    So, if a response of a request with the `accept-encoding: gzip' is cached, none of the
%%    abovementionned variations would result in returning the cached response. When set to
%%    `true', this options allows automatically returning acceptable content when available
%%    even when headers don't exactly match.
%%
%%    Doesn't take priority into account (except for priority 0 which is discarded).
%%
%%    Used by {@link get/2}.  Defaults to `false'.
%%  </li>
%%  <li>
%%    `auto_compress': automatically compresses decompressed text responses with gzip.
%%    This can help with reducing the size of stored content. Moreover, most browsers
%%    do support gzip encoding.
%%
%%    When this option is used, `auto_decompress' is automatically set to `true' as well.
%%
%%    Does not compress responses with strong etags (see
%%    [https://bz.apache.org/bugzilla/show_bug.cgi?id=63932]).
%%
%%    Used by {@link cache/3}.  Defaults to `false'.
%%  </li>
%%  <li>
%%    `auto_compress_mime_types': the list of mime-types that are compressed when
%%    `auto_compress' is used.
%%
%%    Used by {@link cache/3}.  Defaults to
%%    `[<<"text/html">>, <<"text/css">>, <<"text/plain">>, <<"text/xml">>, <<"text/javascript">>, <<"application/javascript">>, <<"application/json">>, <<"application/ld+json">>, <<"application/xml">>, <<"application/xhtml+xml">>, <<"application/rss+xml">>, <<"application/atom+xml">>, <<"image/svg+xml">>, <<"font/ttf">>, <<"font/eot">>, <<"font/otf">>, <<"font/opentype">> ]'
%%  </li>
%%  <li>
%%    `auto_decompress': automatically decompresses stored gzip responses when the client
%%    does not support compression.
%%
%%    Does not decompress responses with strong etags (see
%%    [https://bz.apache.org/bugzilla/show_bug.cgi?id=63932]).
%%
%%    Used by {@link get/2}.  Defaults to `false'.
%%  </li>
%%  <li>
%%    `bucket': an Erlang term to differentiate between different caches. For instance,
%%    if using several shared caches together, this option can be used to differentiate
%%    the cached responses and prevent them from being mixed up, potentially leaking
%%    private data.
%%    Used by {@link get/2} and {@link cache/3}. Defaults to the atom `default'.
%%  </li>
%%  <li>
%%    `cache_disconnected': indicates that the current cache using this library is unable to
%%    reach the origin server. In this case, a stale response can be returned even if the
%%    HTTP cache headers do not explicitely allow it.
%%    Used by {@link get/2}. Defaults to `false'.
%%  </li>
%%  <li>
%%    `default_ttl': the default TTL, in seconds. This value is used when no TTL information
%%    is found in the response, but the response is cacheable by default (see
%%    [https://datatracker.ietf.org/doc/html/rfc7231#section-6.1]).
%%    Used by {@link cache/3}. Defaults to `120'.
%%  </li>
%%  <li>
%%    `default_grace': the amount of time an expired response is kept in the cache. Such
%%    a response is called a stale response, and can be returned in some circumstances, for
%%    instance when the origin server returns an `5xx' error.
%%    Use by {@link cache/3}. Defaults to `120'.
%%  </li>
%%  <li>
%%    `ignore_query_params_order': when a response is cached, a request key is computed based
%%    on the method, URL and body. This option allows to keep the same request key for URLs
%%    whose parameters are identical, but in different order. This helps increasing cache hit if
%%    URL parameter order doesn't matter.
%%    Used by {@link get/2} and {@link cache/3}. Defaults to `false'.
%%  </li>
%%  <li>
%%    `max_ranges': maximum number of range sets accepted when responding to a range request.
%%    This is limited to avoid DOS attack by a client.
%%    See [https://datatracker.ietf.org/doc/html/rfc7233#section-6.1].
%%    Used by {@link get/2}. Defaults to `100'.
%%  </li>
%%  <li>
%%    `store': the store backend's module name.
%%    Used by all functions, no defaults.
%%  </li>
%%  <li>
%%    `type': cache type. A CDN is an example of a shared cache. A browser cache is an example
%%    of a private cache.
%%    Used by {@link get/2} and {@link cache/3}. Defaults to `shared'.
%%  </li>
%%  <li>
%%    `request_time': the time the request was initiated. Setting this timestamp helps correcting
%%    the age of the request between the time the request was made and the time the response
%%    was received and cached, which can be several seconds.
%%    Used by {@link cache/3}.
%%  </li>
%% </ul>
-type opt() ::
    {alternate_keys, [alternate_key()]} |
    {allow_stale_while_revalidate, boolean()} |
    {allow_stale_if_error, boolean()} |
    {auto_accept_encoding, boolean()} |
    {auto_compress, boolean()} |
    {auto_decompress, boolean()} |
    {bucket, term()} |
    {cache_disconnected, boolean()} |
    {default_ttl, non_neg_integer() | none} |
    {default_grace, non_neg_integer() | none} |
    {ignore_query_params_order, boolean()} |
    {max_ranges, non_neg_integer()} |
    {store, module()} |
    {type, type()} |
    {request_time, non_neg_integer()}.

    %TODO: rename to origin_unreachable

-type invalidation_result() ::
    {ok, NbInvalidation :: non_neg_integer() | undefined} | {error, term()}.

%% `undefined' is returned if the backend does not support counting the number of
%% invalidated responses.
%%
%% `{error, term()}' is returned by the backend in case of error.

-define(DEFAULT_TTL, 2 * 60).
-define(DEFAULT_GRACE, 2 * 60).
-define(DEFAULT_COMPRESS_MIME_TYPES,
        #{{<<"text">>, <<"html">>} => [],
          {<<"text">>, <<"css">>} => [],
          {<<"text">>, <<"plain">>} => [],
          {<<"text">>, <<"xml">>} => [],
          {<<"text">>, <<"javascript">>} => [],
          {<<"application">>, <<"javascript">>} => [],
          {<<"application">>, <<"json">>} => [],
          {<<"application">>, <<"ld+json">>} => [],
          {<<"application">>, <<"xml">>} => [],
          {<<"application">>, <<"html+xml">>} => [],
          {<<"application">>, <<"rss+xml">>} => [],
          {<<"application">>, <<"atom+xml">>} => [],
          {<<"image">>, <<"svg+xml">>} => [],
          {<<"font">>, <<"ttf">>} => [],
          {<<"font">>, <<"eot">>} => [],
          {<<"font">>, <<"otf">>} => [],
          {<<"font">>, <<"opentype">>} => []}).
-define(CSL_HEADERS,
        #{<<"accept-encoding">> => [],
          <<"cache-control">> => [],
          <<"content-encoding">> => [],
          <<"if-none-match">> => [],
          <<"vary">> => [],
          <<"warning">> => []}).
-define(KV_HEADERS, #{<<"accept-encoding">> => [], <<"cache-control">> => []}).
-define(DEFAULT_OPTS,
        #{allow_stale_while_revalidate => false,
          allow_stale_if_error => false,
          alternate_keys => [],
          auto_accept_encoding => false,
          auto_compress => false,
          auto_decompress => false,
          bucket => default,
          cache_disconnected => false,
          default_ttl => ?DEFAULT_TTL,
          default_grace => ?DEFAULT_GRACE,
          ignore_query_params_order => false,
          max_ranges => 100,
          type => shared}).
-define(TELEMETRY_LOOKUP_EVT, [http_cache, lookup]).
-define(TELEMETRY_CACHE_EVT, [http_cache, cache]).
-define(TELEMETRY_INVALIDATION_EVT, [http_cache, invalidation]).
-define(TELEMETRY_COMPRESS_EVT, [http_cache, compress_operation]).
-define(TELEMETRY_DECOMPRESS_EVT, [http_cache, decompress_operation]).

%%====================================================================
%% API functions
%%====================================================================

%%------------------------------------------------------------------------------
%% @doc Gets a response from the cache for the given answer
%%
%% The function returns one of:
%% <dl>
%%    <dt>`{ok, {response_ref(), response()}}'</dt>
%%    <dd>
%%    The response is fresh and can be returned directly to the client.
%%
%%    A `504' response is returned when the `only-if-cached' cache
%%    control request header is used and no suitable response was found.
%%    In this case the request reference is set to `not_found'
%%    </dd>
%%    <dt>`{stale, {response_ref(), response()}}'</dt>
%%    <dd>
%%    The response is stale but can be directly returned to the client.
%%
%%    Stale responses that are cached but cannot be returned do to
%%    unfulfilled condition are not returned.
%%
%%    By default, a stale response is returned only when there's a
%%    `max-stale' header in the request. See the following option to enable
%%    returning stale response in other cases:
%%      <ul>
%%        <li>`allow_stale_while_revalidate'</li>
%%        <li>`allow_stale_if_error'</li>
%%        <li>`cache_disconnected'</li>
%%      </ul>
%%    </dd>
%%    <dt>`{must_revalidate, {response_ref(), response()}}'</dt>
%%    <dd>
%%    The response must be revalidated.
%%    </dd>
%%    <dt>`undefined'</dt>
%%    <dd>No suitable response was found.</dd>
%% </dl>
%%
%% Using this function does not automatically updates the last used time of the
%% object in the cache, because a returned response may or may not be returned to
%% the caller. Therefore, use {@link notify_use/2} with the
%% returned response reference when a cached response is used.
%%
%% @end
%%------------------------------------------------------------------------------

-spec get(request(), opts()) ->
             {ok, {response_ref(), response()}} |
             {stale, {response_ref(), response()}} |
             {must_revalidate, {response_ref(), response()}} |
             undefined.
  %TODO: response ref probably unnecessary
  %TODO: rename rfreshness to fresh | stale | must_revalidate | miss

get(Request, Opts) ->
    do_get(Request, normalize_opts(Opts)).

do_get({_Method, _Url, ReqHeaders, _ReqBody} = Request, #{store := Store} = Opts) ->
    StartTime = now_monotonic_us(),
    RequestKey = request_key(Request, Opts),
    {StoreLookupDur, Candidates} = timer:tc(Store, list_candidates, [RequestKey]),
    ParsedReqHeaders =
        parse_headers(ReqHeaders,
                      [<<"cache-control">>,
                       <<"pragma">>,
                       <<"accept-encoding">>,
                       <<"range">>,
                       <<"if-none-match">>,
                       <<"if-modified-since">>,
                       <<"if-range">>]),
    {CandidateSelectionDur, MaybeCandidate} =
        timer:tc(fun() ->
                    select_candidate(ReqHeaders, ParsedReqHeaders, Candidates, undefined, Opts)
                 end),
    Measurements =
        #{store_lookup_time => StoreLookupDur,
          response_selection_time => CandidateSelectionDur,
          candidate_count => length(Candidates)},
    case MaybeCandidate of
        {Freshness, {RespRef, _Status, _RespHeaders, _VaryHeaders, _RespMetadata}} ->
            case Store:get_response(RespRef) of
                {Status, RespHeaders, _, _} = Response ->
                    case Freshness of
                        fresh ->
                            postprocess_response(ok,
                                                 Request,
                                                 ParsedReqHeaders,
                                                 RespRef,
                                                 Response,
                                                 StartTime,
                                                 Measurements,
                                                 Opts);
                        stale ->
                            postprocess_response(stale,
                                                 Request,
                                                 ParsedReqHeaders,
                                                 RespRef,
                                                 Response,
                                                 StartTime,
                                                 Measurements,
                                                 Opts);
                        must_revalidate ->
                            telemetry:execute(?TELEMETRY_LOOKUP_EVT,
                                              maps:put(total_time,
                                                       now_monotonic_us() - StartTime,
                                                       Measurements),
                                              #{freshness => must_revalidate}),
                            {must_revalidate, {RespRef, {Status, RespHeaders, undefined}}}
                    end;
                undefined ->
                    throw(resp_body_not_accessible)
            end;
        undefined ->
            telemetry:execute(?TELEMETRY_LOOKUP_EVT,
                              maps:put(total_time, now_monotonic_us() - StartTime, Measurements),
                              #{freshness => undefined}),
            undefined
    end.

postprocess_response(Freshness,
                     Request,
                     ParsedReqHeaders,
                     RespRef,
                     {Status, RespHeaders, Body, RespMetadata},
                     StartTime,
                     Measurements,
                     Opts) ->
    Response0 = {Status, RespHeaders, Body},
    case handle_conditions(Request, ParsedReqHeaders, Response0, RespMetadata) of
        {304, _, _} = Response1 ->
            telemetry:execute(?TELEMETRY_LOOKUP_EVT,
                              maps:put(total_time, now_monotonic_us() - StartTime, Measurements),
                              #{freshness => Freshness}),
            {Freshness, {RespRef, Response1}};
        Response1 ->
            Response2 = set_cached_resp_headers(Response1, RespMetadata),
            {Response3, TransformMeasurements} =
                transform_response(Request, ParsedReqHeaders, Response2, RespMetadata, Opts),
            Response4 = handle_file_body(Response3),
            telemetry:execute(?TELEMETRY_LOOKUP_EVT,
                              maps:put(total_time,
                                       now_monotonic_us() - StartTime,
                                       maps:merge(Measurements, TransformMeasurements)),
                              #{freshness => Freshness}),
            {Freshness, {RespRef, Response4}}
    end.

transform_response(Request, ParsedReqHeaders, Response0, RespMetadata, Opts) ->
    {DecompressDur, Response1} =
        timer:tc(fun() -> handle_auto_decompress(ParsedReqHeaders, Response0, RespMetadata, Opts)
                 end),
    {RangeDur, Response2} =
        timer:tc(fun() ->
                    handle_range_request(Request, ParsedReqHeaders, Response1, RespMetadata, Opts)
                 end),
    {Response2, #{decompress_time => DecompressDur, range_time => RangeDur}}.

%%------------------------------------------------------------------------------
%% @doc Notifies the backend that a response was used
%%
%% Some backends, such as LRU backends, need to update metadata (in that case:
%% last used time) when a response is used.
%% @end
%%------------------------------------------------------------------------------
-spec notify_use(http_cache:response_ref(), opts()) -> ok | {error, term()}.
notify_use(RespRef, Opts) ->
    #{store := Store} = normalize_opts(Opts),
    Store:notify_resp_used(RespRef, unix_now()).

%%------------------------------------------------------------------------------
%% @doc Caches a response
%%
%% This function never returns an error, even when the backend store returns one.
%% Instead it returns `{ok, response()}' when the response is cacheable (even
%% if an error occurs to actually save it) or `not_cacheable' when the response
%% cannot be cached. When `{ok, response()}' is returned, the response must be
%% returned to the client, because it can be modified depending on which options
%% are enabled. For example, even an uncompressed text response must be returned
%% with a `Vary: Content-Encoding' header when auto compression is enabled.
%%
%% The returned response is subject to all the configured transformation
%% (compression, range...).
%%
%% This function shall be called with any response, even those known to be not
%% cacheable, such as those of a `DELETE' request, because such non-cacheable request
%% can still have side effects on other cached objects
%% (see [https://datatracker.ietf.org/doc/html/rfc7234#section-4.4]).
%% In this example, a successful `DELETE' request must trigger the
%% invalidation of cached results of the deleted object at the same URL.
%%
%% @end
%%------------------------------------------------------------------------------

-spec cache(request(), response(), opts()) -> {ok, response()} | not_cacheable.
cache(Request, Response, Opts) ->
    analyze_cache(Request, Response, normalize_opts(Opts)).

analyze_cache({Method, _Url, ReqHeaders, _ReqBody} = Request,
              {Status, RespHeaders, _RespBody} = Response,
              Opts) ->
    StartTime = now_monotonic_us(),
    ParsedRespHeaders =
        parse_headers(RespHeaders,
                      [<<"age">>,
                       <<"cache-control">>,
                       <<"content-encoding">>,
                       <<"content-type">>,
                       <<"date">>,
                       <<"etag">>,
                       <<"expires">>,
                       <<"last-modified">>,
                       <<"pragma">>,
                       <<"vary">>]),
    case must_invalidate_request_uri(Request, Response, ParsedRespHeaders) of
        true ->
            handle_invalidation_of_unsafe_method(Request, RespHeaders, Opts),
            telemetry:execute(?TELEMETRY_CACHE_EVT,
                              #{total_time => now_monotonic_us() - StartTime},
                              #{cacheable => false}),
            not_cacheable;
        false ->
            ParsedReqHeaders =
                parse_headers(ReqHeaders, [<<"authorization">>, <<"cache-control">>, <<"pragma">>]),

            case is_cacheable(Method, ParsedReqHeaders, Status, ParsedRespHeaders, Opts) of
                true ->
                    do_cache(Request,
                             ParsedReqHeaders,
                             Response,
                             ParsedRespHeaders,
                             StartTime,
                             #{analysis_time => now_monotonic_us() - StartTime},
                             Opts);
                false ->
                    telemetry:execute(?TELEMETRY_CACHE_EVT,
                                      #{total_time => now_monotonic_us() - StartTime},
                                      #{cacheable => false}),
                    not_cacheable
            end
    end.

do_cache({Method, Url, ReqHeaders0, ReqBody},
         ParsedReqHeaders0,
         {Status, RespHeaders0, RespBody},
         #{<<"content-type">> := {MainType, SubType, _}} = ParsedRespHeaders0,
         StartTime,
         Measurements,
         #{auto_compress := true} = Opts)
    when not is_map_key(<<"content-encoding">>, ParsedRespHeaders0)
         andalso is_map_key({MainType, SubType}, ?DEFAULT_COMPRESS_MIME_TYPES)
         andalso % do not compress response with strong etag
                 % %TODO: use macro for this 3x guard?
                 (not is_map_key(<<"etag">>, ParsedRespHeaders0)
                  orelse element(1, map_get(<<"etag">>, ParsedRespHeaders0)) == weak) ->
    ReqHeaders =
        delete_header(<<"accept-encoding">>, ReqHeaders0)
        ++ [{<<"accept-encoding">>, <<"gzip">>}],
    ParsedReqHeaders =
        maps:merge(ParsedReqHeaders0, parse_headers(ReqHeaders, [<<"accept-encoding">>])),
    RespHeaders =
        set_header_value(<<"content-encoding">>,
                         <<"gzip">>,
                         set_header_value(<<"vary">>, <<"accept-encoding">>, RespHeaders0)),
    ParsedRespHeaders =
        maps:merge(ParsedRespHeaders0,
                   parse_headers(RespHeaders, [<<"content-encoding">>, <<"vary">>])),
    {GzipDur, GzippedBody} = timer:tc(zlib, gzip, [RespBody]),
    telemetry:execute(?TELEMETRY_COMPRESS_EVT, #{duration => GzipDur}, #{alg => gzip}),
    do_cache({Method, Url, ReqHeaders, ReqBody},
             ParsedReqHeaders,
             {Status, RespHeaders, GzippedBody},
             ParsedRespHeaders,
             StartTime,
             maps:put(compress_time, GzipDur, Measurements),
             Opts);
do_cache({_Method, Url, ReqHeaders0, _ReqBody} = Request,
         ParsedReqHeaders,
         {Status, RespHeaders0, RespBody0},
         ParsedRespHeaders,
         StartTime,
         Measurements,
         #{store := Store} = Opts) ->
    RespBodyBin = iolist_to_binary(RespBody0),
    % the response doesn't necessarily has the content length set, but we have this
    % information, so let's be a good citizen of the web and always set it
    RespHeaders1 =
        set_header_value(<<"content-length">>,
                         list_to_binary(integer_to_list(byte_size(RespBodyBin))),
                         RespHeaders0),
    RequestKey = request_key(Request, Opts),
    VaryHeaders = vary_headers(ReqHeaders0, ParsedRespHeaders),
    MaybeAge = maps:get(<<"age">>, ParsedRespHeaders, undefined),
    MaybeDate = maps:get(<<"date">>, ParsedRespHeaders, undefined),
    {TTLSetBy, Expires} = expires(MaybeDate, ParsedRespHeaders, Opts),
    UrlDigest = url_digest(Url, Opts),
    Response = {Status, RespHeaders1, RespBodyBin},
    RespMetadata =
        #{created => created_at(MaybeAge, MaybeDate, Opts),
          expires => Expires,
          grace => grace(Expires, Opts),
          ttl_set_by => TTLSetBy,
          %TODO: keep only those which are really needed
          parsed_headers => ParsedRespHeaders},
    AlternateKeys = map_get(alternate_keys, Opts),
    {StoreDur, StoreRes} =
        timer:tc(Store,
                 put,
                 [RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata, AlternateKeys]),
    case StoreRes of
        %TODO: logging
        ok ->
            ok;
        {error, _Reason} ->
            ok
    end,
    {TransformedResponse, TransformMeasurements} =
        transform_response(Request, ParsedReqHeaders, Response, RespMetadata, Opts),
    AllMeasurements =
        maps:merge(#{total_time => now_monotonic_us() - StartTime, store_save_time => StoreDur},
                   maps:merge(Measurements, TransformMeasurements)),
    telemetry:execute(?TELEMETRY_CACHE_EVT, AllMeasurements, #{cacheable => true}),
    {ok, TransformedResponse}.

%%------------------------------------------------------------------------------
%% @doc Invalidates all responses for a URL
%% @end
%%------------------------------------------------------------------------------
-spec invalidate_url(url(), opts()) -> invalidation_result().
invalidate_url(Url, Opts) ->
    do_invalidate_url(Url, normalize_opts(Opts)).

do_invalidate_url(Url, #{store := Store} = Opts) ->
    UrlDigest = url_digest(Url, Opts),
    {InvalidationDur, InvalidationRes} = timer:tc(Store, invalidate_url, [UrlDigest]),
    log_invalidation_result(InvalidationRes, url, InvalidationDur).

%%------------------------------------------------------------------------------
%% @doc Invalidates all responses stored with the alternate key
%% @end
%%------------------------------------------------------------------------------
-spec invalidate_by_alternate_key(alternate_key() | [alternate_key()], opts()) ->
                                     invalidation_result().
invalidate_by_alternate_key(AltKeys, Opts) ->
    do_invalidate_by_alternate_key(AltKeys, normalize_opts(Opts)).

do_invalidate_by_alternate_key([], _Opts) ->
    {ok, 0};
do_invalidate_by_alternate_key([_ | _] = AltKeys, #{store := Store}) ->
    {InvalidationDur, InvalidationRes} =
        timer:tc(Store, invalidate_by_alternate_key, [AltKeys]),
    log_invalidation_result(InvalidationRes, alternate_key, InvalidationDur);
do_invalidate_by_alternate_key(AltKey, #{store := _} = Opts) ->
    do_invalidate_by_alternate_key([AltKey], Opts).

%%====================================================================
%% Internal functions
%%====================================================================

%%%-------------------------------------------------------------------
%% @doc http cache.
%% @end
%%%-------------------------------------------------------------------

select_candidate(_ReqHeaders,
                 _ParsedReqHeaders,
                 [],
                 SelectedCandidateOrUndefined,
                 _Opts) ->
    SelectedCandidateOrUndefined;
select_candidate(ReqHeaders, ParsedReqHeaders, [Candidate | Rest], undefined, Opts) ->
    case varying_headers_match(ReqHeaders, Candidate, Opts) of
        true ->
            select_candidate(ReqHeaders,
                             ParsedReqHeaders,
                             Rest,
                             {candidate_freshness(Candidate, ParsedReqHeaders, Opts), Candidate},
                             Opts);
        false ->
            select_candidate(ReqHeaders, ParsedReqHeaders, Rest, undefined, Opts)
    end;
select_candidate(ReqHeaders,
                 ParsedReqHeaders,
                 [Candidate | Rest],
                 {SelectedFreshness, SelectedResp},
                 Opts) ->
    case varying_headers_match(ReqHeaders, Candidate, Opts) of
        true ->
            case {SelectedFreshness, candidate_freshness(Candidate, ParsedReqHeaders, Opts)} of
                {Freshness, Freshness} ->
                    BestCandidate = freshest_candidate(SelectedResp, Candidate),
                    select_candidate(ReqHeaders,
                                     ParsedReqHeaders,
                                     Rest,
                                     {Freshness, BestCandidate},
                                     Opts);
                {fresh, _StaleOrMustRevalidate} ->
                    select_candidate(ReqHeaders,
                                     ParsedReqHeaders,
                                     Rest,
                                     {fresh, SelectedResp},
                                     Opts);
                {_StaleOrMustRevalidate, fresh} ->
                    select_candidate(ReqHeaders, ParsedReqHeaders, Rest, {fresh, Candidate}, Opts);
                {stale, must_revalidate} ->
                    select_candidate(ReqHeaders,
                                     ParsedReqHeaders,
                                     Rest,
                                     {stale, SelectedResp},
                                     Opts);
                {must_revalidate, stale} ->
                    select_candidate(ReqHeaders, ParsedReqHeaders, Rest, {stale, Candidate}, Opts)
            end;
        false ->
            select_candidate(ReqHeaders,
                             ParsedReqHeaders,
                             Rest,
                             {SelectedFreshness, SelectedResp},
                             Opts)
    end.

varying_headers_match(ReqHeaders,
                      {_Key, _Status, _Headers, RespVaryHeaders, RespMetadata},
                      Opts) ->
    NormReqHeaders = normalize_headers(ReqHeaders, maps:keys(RespVaryHeaders)),
    lists:all(fun({RespHeaderName, RespHeaderValue}) ->
                 varying_header_match(NormReqHeaders,
                                      RespHeaderName,
                                      RespHeaderValue,
                                      RespMetadata,
                                      Opts)
              end,
              maps:to_list(RespVaryHeaders)).

varying_header_match(#{<<"accept-encoding">> := undefined},
                     <<"accept-encoding">>,
                     _RespHeaderValue,
                     #{parsed_headers :=
                           #{<<"content-encoding">> := [<<"gzip">>]} = ParsedRespHeaders},
                     #{auto_decompress := true})
    when not is_map_key(<<"etag">>, ParsedRespHeaders)
         orelse element(1, map_get(<<"etag">>, ParsedRespHeaders)) == weak ->
    true;
% we do not support multiple successive encodings
varying_header_match(%TODO: use parsed request headers?
                     #{<<"accept-encoding">> := <<_/binary>> = AcceptEncoding},
                     <<"accept-encoding">>,
                     _RespHeaderValue,
                     #{parsed_headers := #{<<"content-encoding">> := [ContentEncoding]}},
                     #{auto_accept_encoding := true}) ->
    AcceptedEncodings =
        [Encoding
         || {Encoding, Priority} <- cow_http_hd:parse_accept_encoding(AcceptEncoding),
            Priority > 0],
    lists:member(ContentEncoding, AcceptedEncodings);
varying_header_match(NormReqHeaders,
                     RespHeaderName,
                     RespHeaderValue,
                     _RespMetadata,
                     _Opts) ->
    maps:get(RespHeaderName, NormReqHeaders, undefined) == RespHeaderValue.

freshest_candidate({_, _, _, _, #{created := CLCreated}} = CL,
                   {_, _, _, _, #{created := CRCreated}} = _CR)
    when CLCreated > CRCreated ->
    CL;
freshest_candidate(_CL, CR) ->
    CR.

candidate_freshness({_, _, _, _, #{parsed_headers := ParsedRespHeaders}} = Candidate,
                    ParsedReqHeaders,
                    Opts) ->
    MustRevalidate =
        not no_cache_satisfied(ParsedReqHeaders)
        orelse not no_cache_satisfied(ParsedRespHeaders)
        orelse not req_max_age_satisfied(Candidate, ParsedReqHeaders)
        orelse not req_min_fresh_satisfied(Candidate, ParsedReqHeaders)
        orelse not is_fresh(Candidate) andalso resp_must_revalidate(ParsedRespHeaders)
        orelse not is_fresh(Candidate)
               andalso resp_proxy_must_revalidate(ParsedRespHeaders, Opts),

    case MustRevalidate of
        true ->
            must_revalidate;
        false ->
            case is_fresh(Candidate) of
                true ->
                    fresh;
                false ->
                    CanBeServedStale =
                        req_max_stale_satisfied(Candidate, ParsedReqHeaders)
                        orelse stale_if_error_satisfied(Candidate, ParsedReqHeaders, Opts)
                        orelse stale_if_error_satisfied(Candidate, ParsedRespHeaders, Opts)
                        orelse resp_stale_while_revalidate_satisfied(Candidate,
                                                                     ParsedRespHeaders,
                                                                     Opts)
                        orelse map_get(cache_disconnected, Opts),

                    case CanBeServedStale of
                        true ->
                            stale;
                        false ->
                            must_revalidate
                    end
            end
    end.

is_fresh(Candidate) ->
    is_fresh(Candidate, unix_now()).

is_fresh({_, _, _, _, #{expires := Expires}}, Now) when Now < Expires ->
    true;
is_fresh(_, _) ->
    false.

no_cache_satisfied(#{<<"cache-control">> := #{<<"no-cache">> := _}}) ->
    false;
% pragma header must be ignored if the cache-control header was successfully parsed
no_cache_satisfied(#{<<"pragma">> := no_cache, <<"cache-control">> := _}) ->
    true;
no_cache_satisfied(#{<<"pragma">> := no_cache}) ->
    false;
no_cache_satisfied(_) ->
    true.

req_max_age_satisfied(Candidate, ParsedReqHeaders) ->
    req_max_age_satisfied(Candidate, ParsedReqHeaders, unix_now()).

req_max_age_satisfied({_, _, _, _, #{created := Created}},
                      #{<<"cache-control">> := #{<<"max-age">> := MaxAgeDur, <<"max-stale">> := _}},
                      Now)
    when Now - Created < MaxAgeDur ->
    true;
req_max_age_satisfied({_, _, _, _, #{created := Created, expires := Expires}},
                      #{<<"cache-control">> := #{<<"max-age">> := MaxAgeDur}},
                      Now)
    when Now - Created < MaxAgeDur andalso Now < Expires ->
    true;
req_max_age_satisfied(_Candidate,
                      #{<<"cache-control">> := #{<<"max-age">> := _MaxAgeDur}},
                      _Now) ->
    false;
req_max_age_satisfied(_Candidate, _ParsedReqHeaders, _Now) ->
    true.

req_min_fresh_satisfied(Candidate, ParsedReqHeaders) ->
    req_min_fresh_satisfied(Candidate, ParsedReqHeaders, unix_now()).

req_min_fresh_satisfied({_, _, _, _, #{expires := Expires}},
                        #{<<"cache-control">> := #{<<"min-fresh">> := MinFreshDur}},
                        Now)
    when Expires - Now < MinFreshDur ->
    false;
req_min_fresh_satisfied(_, _, _) ->
    true.

req_max_stale_satisfied(Candidate, ParsedReqHeaders) ->
    req_max_stale_satisfied(Candidate, ParsedReqHeaders, unix_now()).

req_max_stale_satisfied({_, _, _, _, #{expires := Expires}},
                        #{<<"cache-control">> := #{<<"max-stale">> := MaxStaleDur}},
                        Now)
    when is_integer(MaxStaleDur) andalso Now - Expires < MaxStaleDur ->
    true;
req_max_stale_satisfied(_, _, _) ->
    false.

stale_if_error_satisfied(Candidate, ParsedHeaders, Opts) ->
    stale_if_error_satisfied(Candidate, ParsedHeaders, Opts, unix_now()).

stale_if_error_satisfied({_, _, _, _, #{expires := Expires}},
                         #{<<"cache-control">> := #{<<"stale-if-error">> := StaleIfErrorDur}},
                         #{allow_stale_if_error := true},
                         Now)
    when Now - Expires < StaleIfErrorDur ->
    true;
stale_if_error_satisfied(_, _, _, _) ->
    false.

resp_stale_while_revalidate_satisfied(Candidate, ParsedRespHeaders, Opts) ->
    resp_stale_while_revalidate_satisfied(Candidate, ParsedRespHeaders, Opts, unix_now()).

resp_stale_while_revalidate_satisfied({_, _, _, _, #{expires := Expires}},
                                      #{<<"cache-control">> :=
                                            #{<<"stale-while-revalidate">> :=
                                                  StaleWhileRevalidateDur}},
                                      #{allow_stale_while_revalidate := true},
                                      Now)
    when Now - Expires < StaleWhileRevalidateDur ->
    true;
resp_stale_while_revalidate_satisfied(_, _, _, _) ->
    false.

resp_must_revalidate(#{<<"cache-control">> := #{<<"must-revalidate">> := _}}) ->
    true;
resp_must_revalidate(_) ->
    false.

resp_proxy_must_revalidate(#{<<"cache-control">> := #{<<"proxy-revalidate">> := _}},
                           #{type := shared}) ->
    true;
resp_proxy_must_revalidate(_, _) ->
    false.

set_cached_resp_headers({Status, RespHeaders0, RespBody}, RespMetadata) ->
    RespHeaders1 = set_age_header(RespHeaders0, RespMetadata),
    RespHeaders2 = set_warning_header(RespHeaders1, RespMetadata),
    {Status, RespHeaders2, RespBody}.

set_age_header(Headers, #{created := Created}) ->
    Age = unix_now() - Created,
    set_header_value(<<"age">>,
                     list_to_binary(integer_to_list(Age)),
                     delete_header(<<"age">>, Headers)).

set_warning_header(RespHeaders, RespMetadata) ->
    Now = unix_now(),
    Warnings =
        [Warning
         || Warning
                <- [response_stale_warning(RespMetadata, Now),
                    response_heuristic_expiration_warning(RespMetadata, Now)],
            Warning /= undefined],
    case Warnings of
        [] ->
            RespHeaders;
        Warnings ->
            WarningHeader = iolist_to_binary(lists:join(<<", ">>, Warnings)),
            set_header_value(<<"warning">>, WarningHeader, RespHeaders)
    end.

response_stale_warning(#{expires := Expires}, Now) when Now >= Expires ->
    <<"110 - \"response is Stale\"">>;
response_stale_warning(_RespMetadata, _Now) ->
    undefined.

response_heuristic_expiration_warning(#{created := CreatedAt, ttl_set_by := heuristics},
                                      Now)
    when Now - CreatedAt > 24 * 60 * 60 ->
    <<"113 - \"heuristic expiration\"">>;
response_heuristic_expiration_warning(_RespMetadata, _Now) ->
    undefined.

handle_file_body({Status, RespHeaders, {file, FilePath}}) ->
    {Status, RespHeaders, {sendfile, 0, all, FilePath}};
handle_file_body(Response) ->
    Response.

request_key({Method, Url, _Headers, Body}, Opts) ->
    % method is case-sensitive, no need to normalize
    MethodDigest = crypto:hash(sha256, Method),
    UrlDigest = url_digest(Url, Opts),
    BodyDigest = crypto:hash(sha256, iolist_to_binary(Body)),
    BucketDigest = crypto:hash(sha256, erlang:term_to_binary(map_get(bucket, Opts))),
    crypto:hash(sha256,
                <<MethodDigest/binary, UrlDigest/binary, BodyDigest/binary, BucketDigest/binary>>).

url_digest(Url, Opts) ->
    NormUrl = uri_string:normalize(Url),
    #{host := Host, path := Path} = ParsedUrl = uri_string:parse(NormUrl),
    NormQuery = normalize_query(ParsedUrl, Opts),
    BucketDigest = crypto:hash(sha256, erlang:term_to_binary(map_get(bucket, Opts))),
    crypto:hash(sha256, <<Host/binary, Path/binary, NormQuery/binary, BucketDigest/binary>>).

normalize_query(#{query := Query}, #{ignore_query_params_order := true}) ->
    DissectedQuery = uri_string:dissect_query(Query),
    OrderedQuery = lists:sort(DissectedQuery),
    uri_string:compose_query(OrderedQuery);
normalize_query(#{query := Query}, _Opts) ->
    Query;
normalize_query(_ParsedUrl, _Opts) ->
    <<"">>.

normalize_headers(Headers, WhichHeaders) ->
    maps:merge(
        maps:from_keys(WhichHeaders, undefined), normalize_headers(Headers, WhichHeaders, #{})).

normalize_headers([], _WhichHeaders, NormHeaders) ->
    NormHeaders;
normalize_headers([{Name, Value} | Rest], WhichHeaders, NormHeaders) ->
    NormName = normalize_header_name(Name),
    case lists:member(NormName, WhichHeaders) of
        true ->
            NormValue = normalize_header_value(Value),
            case NormHeaders of
                #{NormName := AccumulatedValue} ->
                    NewValue = <<AccumulatedValue/binary, ", ", NormValue/binary>>,
                    normalize_headers(Rest,
                                      WhichHeaders,
                                      maps:put(NormName, NewValue, NormHeaders));
                _ ->
                    normalize_headers(Rest,
                                      WhichHeaders,
                                      maps:put(NormName, NormValue, NormHeaders))
            end;
        false ->
            normalize_headers(Rest, WhichHeaders, NormHeaders)
    end.

normalize_header_name(HeaderName) ->
    string:lowercase(
        string:trim(HeaderName, both, " ")).

normalize_header_value(HeaderValue) ->
    string:trim(HeaderValue, both, " \t").

normalize_opts(Opts) ->
    case proplists:get_value(store, Opts, undefined) of
        undefined ->
            erlang:error(no_store_configured);
        _ ->
            ok
    end,
    UserOpts = proplists:to_map(Opts),
    NormUserOpts =
        case UserOpts of
            #{auto_compress := true} ->
                maps:put(auto_decompress, true, UserOpts);
            _ ->
                UserOpts
        end,
    maps:merge(?DEFAULT_OPTS, NormUserOpts).

% Invalidating POST requests is a grey zone: POST requests can be cached when there is
% explicit cache information (RFC7231 section 4.3.3) but POST requests are unsafe and must also
% trigger invalidation of the response for the URI (RFC7234 4.4).
% So we invalidate URI of POST requests only when the reponse is not cacheable
must_invalidate_request_uri({<<"GET">>, _, _, _}, _Response, _) ->
    false;
must_invalidate_request_uri({<<"HEAD">>, _, _, _}, _Response, _) ->
    false;
must_invalidate_request_uri({<<"OPTIONS">>, _, _, _}, _Response, _) ->
    false;
must_invalidate_request_uri({<<"POST">>, _, _, _},
                            _Response,
                            #{<<"cache-control">> := #{<<"s-maxage">> := _}}) ->
    false;
must_invalidate_request_uri({<<"POST">>, _, _, _},
                            _Response,
                            #{<<"cache-control">> := #{<<"max-age">> := _}}) ->
    false;
must_invalidate_request_uri({<<"POST">>, _, _, _}, _Response, #{<<"expires">> := _}) ->
    false;
must_invalidate_request_uri(_Request, {Status, _, _}, _)
    when Status >= 200 andalso Status =< 399 ->
    true;
must_invalidate_request_uri(_Request, _Response, _) ->
    false.

handle_invalidation_of_unsafe_method({_, Url, _, _}, [], #{store := Store} = Opts) ->
    Store:invalidate_url(url_digest(Url, Opts));
handle_invalidation_of_unsafe_method(Request,
                                     [{HeaderName, HeaderValue} | RestHeaders],
                                     #{store := Store} = Opts) ->
    case ?LOWER(HeaderName) of
        <<"location">> ->
            Store:invalidate_url(url_digest(HeaderValue, Opts));
        <<"content-location">> ->
            Store:invalidate_url(url_digest(HeaderValue, Opts));
        _ ->
            ok
    end,
    handle_invalidation_of_unsafe_method(Request, RestHeaders, Opts).

is_cacheable(Method, ParsedReqHeaders, Status, ParsedRespHeaders, Opts) ->
    request_method_cacheable(Method, ParsedRespHeaders, Opts)
    andalso not
                is_map_key(<<"no-store">>, maps:get(<<"cache-control">>, ParsedReqHeaders, #{}))
    andalso not
                is_map_key(<<"no-store">>, maps:get(<<"cache-control">>, ParsedRespHeaders, #{}))
    andalso (cache_type(Opts) /= shared
             orelse not
                        is_map_key(<<"private">>,
                                   maps:get(<<"cache-control">>, ParsedRespHeaders, #{})))
    andalso (cache_type(Opts) /= shared
             orelse not is_map_key(<<"authorization">>, ParsedReqHeaders)
             orelse response_explicitely_cacheable(ParsedRespHeaders))
    andalso (is_map_key(<<"expires">>, ParsedRespHeaders)
             orelse is_map_key(<<"max-age">>, maps:get(<<"cache-control">>, ParsedRespHeaders, #{}))
             orelse (cache_type(Opts) == shared)
                    and is_map_key(<<"s-maxage">>,
                                   maps:get(<<"cache-control">>, ParsedRespHeaders, #{}))
             orelse response_status_cacheable(Status)
             orelse is_map_key(<<"public">>, maps:get(<<"cache-control">>, ParsedRespHeaders, #{})))
    andalso maps:get(<<"vary">>, ParsedRespHeaders, undefined) /= <<"*">>.

request_method_cacheable(<<"GET">>, _, _) ->
    true;
request_method_cacheable(<<"HEAD">>, _, _) ->
    true;
request_method_cacheable(<<"POST">>,
                         #{<<"cache-control">> := #{<<"s-maxage">> := _}},
                         #{type := shared}) ->
    true;
request_method_cacheable(<<"POST">>,
                         #{<<"cache-control">> := #{<<"max-age">> := _}},
                         _) ->
    true;
request_method_cacheable(<<"POST">>, #{<<"expires">> := _}, _) ->
    true;
request_method_cacheable(_, _, _) ->
    false.

response_status_cacheable(200) ->
    true;
response_status_cacheable(203) ->
    true;
response_status_cacheable(204) ->
    true;
response_status_cacheable(300) ->
    true;
response_status_cacheable(301) ->
    true;
response_status_cacheable(404) ->
    true;
response_status_cacheable(405) ->
    true;
response_status_cacheable(410) ->
    true;
response_status_cacheable(414) ->
    true;
response_status_cacheable(451) ->
    true;
response_status_cacheable(501) ->
    true;
response_status_cacheable(_) ->
    false.

response_explicitely_cacheable(#{<<"cache-control">> := #{<<"must-revalidate">> := _}}) ->
    true;
response_explicitely_cacheable(#{<<"cache-control">> := #{<<"public">> := _}}) ->
    true;
response_explicitely_cacheable(#{<<"cache-control">> := #{<<"s-maxage">> := _}}) ->
    true;
response_explicitely_cacheable(_) ->
    false.

vary_headers(ReqHeaders, #{<<"vary">> := HeaderList}) ->
    NormalizedHeaders = normalize_headers(ReqHeaders, HeaderList),
    % even when a header does not appear in the request, it must be taken into account
    % according to the vary header
    maps:merge(
        maps:from_keys(HeaderList, undefined), NormalizedHeaders);
vary_headers(_ReqHeaders, _ParsedRespHeaders) ->
    #{}.

created_at(MaybeAge, MaybeDate, Opts) ->
    case {apparent_age(MaybeDate), corrected_age_value(MaybeAge, Opts)} of
        {undefined, undefined} ->
            unix_now();
        {ApparentAge, undefined} ->
            unix_now() - ApparentAge;
        {undefined, CorrectedAge} ->
            unix_now() - CorrectedAge;
        {ApparentAge, CorrectedAge} ->
            unix_now() - max(ApparentAge, CorrectedAge)
    end.

apparent_age(Date) when is_integer(Date) ->
    max(0, unix_now() - Date);
apparent_age(undefined) ->
    undefined.

corrected_age_value(Age, #{request_time := RequestTime}) when is_integer(Age) ->
    ResponseDelay = unix_now() - RequestTime,
    Age + ResponseDelay;
corrected_age_value(_, _Opts) ->
    undefined.

expires(_MaybeDate,
        #{<<"cache-control">> := #{<<"s-maxage">> := SMaxAge}},
        #{type := shared}) ->
    {header, unix_now() + SMaxAge};
expires(_MaybeDate, #{<<"cache-control">> := #{<<"max-age">> := MaxAge}}, _Opts) ->
    {header, unix_now() + MaxAge};
expires(Date, #{<<"expires">> := Expires}, _Opts) when is_integer(Date) ->
    {header, unix_now() + Expires - Date};
expires(_MaybeDate, #{<<"expires">> := Expires}, _Opts) ->
    {header, Expires};
expires(_MaybeDate, _ParsedRespHeaders, #{default_ttl := DefaultTTL}) ->
    {heuristics, unix_now() + DefaultTTL}.

grace(Expires, #{default_grace := Grace}) ->
    Expires + Grace.

cache_type(#{type := Type}) ->
    Type.

handle_auto_decompress(ParsedReqHeaders,
                       {Status, RespHeaders, Body},
                       #{parsed_headers :=
                             #{<<"content-encoding">> := [<<"gzip">>]} = ParsedRespHeaders},
                       #{auto_decompress := true})
    when not is_map_key(<<"accept-encoding">>, ParsedReqHeaders)
         andalso % no decompressing of responses with strong validator
                 (not is_map_key(<<"etag">>, ParsedRespHeaders)
                  orelse element(1, map_get(<<"etag">>, ParsedRespHeaders)) == weak) ->
    {GunzipDur, DecompressedBody} = timer:tc(zlib, gunzip, [Body]),
    telemetry:execute(?TELEMETRY_DECOMPRESS_EVT, #{duration => GunzipDur}, #{alg => gzip}),
    {Status,
     delete_header(<<"content-encoding">>,
                   set_header_value(<<"content-length">>,
                                    list_to_binary(integer_to_list(byte_size(DecompressedBody))),
                                    RespHeaders)),
     DecompressedBody};
handle_auto_decompress(_ParsedReqHeaders, Response, _RespMetadata, _Opts) ->
    Response.

% cowlib parse empty bytes without raising
handle_range_request({<<"GET">>, _, _, _},
                     #{<<"range">> := {bytes, []}},
                     Response,
                     _RespMetadata,
                     _Opts) ->
    Response;
handle_range_request({<<"GET">>, _, _, _},
                     #{<<"range">> := {bytes, _}, <<"if-range">> := {EtagType, EtagVal}},
                     Response,
                     #{parsed_headers := #{<<"etag">> := {RespEtagType, RespEtagVal}}},
                     _Opts)
    when not
             (EtagType == strong andalso RespEtagType == strong andalso EtagVal == RespEtagVal) ->
    Response;
handle_range_request({<<"GET">>, _, _, _},
                     #{<<"range">> := {bytes, _}, <<"if-range">> := {_EtagType, _EtagVal}},
                     Response,
                     #{parsed_headers := ParsedRespHeaders},
                     _Opts)
    when not is_map_key(<<"etag">>, ParsedRespHeaders) ->
    Response;
handle_range_request({<<"GET">>, _, _, _},
                     #{<<"range">> := {bytes, _}, <<"if-range">> := IfUnmodifiedSince},
                     Response,
                     #{parsed_headers :=
                           #{<<"last-modified">> := LastModified, <<"date">> := Date}},
                     _Opts)
    when is_integer(IfUnmodifiedSince)
         andalso not (Date - LastModified >= 60 andalso LastModified =< IfUnmodifiedSince) ->
    Response;
handle_range_request({<<"GET">>, _, _, _},
                     #{<<"range">> := {bytes, _}, <<"if-range">> := IfUnmodifiedSince},
                     Response,
                     #{parsed_headers := ParsedRespHeaders},
                     _Opts)
    when is_integer(IfUnmodifiedSince)
         andalso not
                     (is_map_key(<<"last-modified">>, ParsedRespHeaders)
                      andalso is_map_key(<<"date">>, ParsedRespHeaders)) ->
    Response;
handle_range_request({<<"GET">>, _, _, _},
                     #{<<"range">> := {bytes, ByteRangeSpec}},
                     Response,
                     RespMetadata,
                     #{max_ranges := MaxRanges}) ->
    BodySize = resp_body_byte_size(Response),
    NormByteRangeSpec = normalize_range_spec(ByteRangeSpec, BodySize),
    if length(NormByteRangeSpec) < MaxRanges ->
           ChunksWithRange = get_range_chunks(NormByteRangeSpec, Response),
           build_range_response(ChunksWithRange, BodySize, Response, RespMetadata);
       true ->
           range_not_satisfiable_resp(BodySize)
    end;
handle_range_request(_Request, _ParsedReqHeaders, Response, _RespMetadata, _Opts) ->
    Response.

resp_body_byte_size({_, _, <<_/binary>> = Body}) ->
    byte_size(Body);
resp_body_byte_size({_, _, {file, FilePath}}) ->
    case file:read_file_info(FilePath) of
        {ok, FileInfo} ->
            FileInfo#file_info.size;
        _ ->
            throw(resp_body_not_accessible)
    end.

normalize_range_spec(RangeSpec, BodySize) ->
    normalize_range_spec(RangeSpec, BodySize, []).

normalize_range_spec([], _BodySize, Acc) ->
    lists:reverse(Acc);
normalize_range_spec([{StartOffset, infinity} | Rest], BodySize, Acc)
    when StartOffset + 1 > BodySize ->
    normalize_range_spec(Rest, BodySize, Acc);
normalize_range_spec([{StartOffset, infinity} | Rest], BodySize, Acc) ->
    normalize_range_spec(Rest, BodySize, [{StartOffset, BodySize - StartOffset} | Acc]);
normalize_range_spec([{StartOffset, EndOffset} | Rest], BodySize, Acc)
    when StartOffset > EndOffset ->
    normalize_range_spec([{EndOffset, StartOffset} | Rest], BodySize, Acc);
normalize_range_spec([{_, EndOffset} | Rest], BodySize, Acc)
    when EndOffset + 1 > BodySize ->
    normalize_range_spec(Rest, BodySize, Acc);
normalize_range_spec([{StartOffset, EndOffset} | Rest], BodySize, Acc) ->
    normalize_range_spec(Rest, BodySize, [{StartOffset, EndOffset - StartOffset + 1} | Acc]);
normalize_range_spec([OffsetFromEnd | Rest], BodySize, Acc)
    when abs(OffsetFromEnd) > BodySize ->
    normalize_range_spec(Rest, BodySize, Acc);
normalize_range_spec([OffsetFromEnd | Rest], BodySize, Acc) ->
    normalize_range_spec(Rest,
                         BodySize,
                         [{BodySize - abs(OffsetFromEnd), abs(OffsetFromEnd)} | Acc]).

get_range_chunks(NormByteRangeSpec, {_, _, {file, Filename}}) ->
    case file:open(Filename, [read, raw, binary]) of
        {ok, File} ->
            case file:pread(File, NormByteRangeSpec) of
                {ok, Chunks} ->
                    file:close(File),
                    lists:zip(NormByteRangeSpec, Chunks);
                {error, _} ->
                    file:close(File),
                    throw(resp_body_not_accessible)
            end;
        {error, _} ->
            throw(resp_body_not_accessible)
    end;
get_range_chunks(NormByteRangeSpec, {_, _, Body}) ->
    [{Range, binary:part(Body, StartOffset, Length)}
     || {StartOffset, Length} = Range <- NormByteRangeSpec].

build_range_response([], BodySize, _Response, _RespMetadata) ->
    range_not_satisfiable_resp(BodySize);
build_range_response([{{StartOffset, Length}, Chunk}],
                     BodySize,
                     {_Status, RespHeaders0, _Body},
                     _RespMetadata) ->
    RespHeaders1 = delete_header(<<"content-length">>, RespHeaders0),
    RespHeaders2 =
        set_header_value(<<"content-range">>,
                         content_range(StartOffset, Length, BodySize),
                         RespHeaders1),
    RespHeaders3 =
        set_header_value(<<"content-length">>,
                         list_to_binary(integer_to_list(Length)),
                         RespHeaders2),
    {206, RespHeaders3, Chunk};
build_range_response([{{FirstStartOffset, FirstLength}, FirstChunk} | OtherChunks],
                     BodySize,
                     {_Status, RespHeaders, _Body},
                     RespMetadata) ->
    PartBaseHeaders =
        case RespMetadata of
            #{parsed_headers := #{<<"content-type">> := ContentType}} ->
                [{<<"content-type">>, parsed_content_type_to_string(ContentType)}];
            _ ->
                []
        end,
    Boundary = cow_multipart:boundary(),
    FirstPart =
        [cow_multipart:first_part(Boundary,
                                  [{<<"content-range">>,
                                    content_range(FirstStartOffset, FirstLength, BodySize)}
                                   | PartBaseHeaders]),
         FirstChunk],
    OtherParts =
        [[cow_multipart:part(Boundary,
                             [{<<"content-range">>, content_range(StartOffset, Length, BodySize)}
                              | PartBaseHeaders]),
          Chunk]
         || {{StartOffset, Length}, Chunk} <- OtherChunks],
    RespHeadersMultipart =
        [{<<"content-type">>, <<"multipart/byteranges; boundary=", Boundary/binary>>}
         | delete_header(<<"content-type">>, RespHeaders)],
    Body = [FirstPart, OtherParts, cow_multipart:close(Boundary)],
    {206, RespHeadersMultipart, Body}.

content_range(StartOffset, Length, BodySize) ->
    BodySizeBin = list_to_binary(integer_to_list(BodySize)),
    StartOffsetBin = list_to_binary(integer_to_list(StartOffset)),
    EndOffsetBin = list_to_binary(integer_to_list(StartOffset + Length - 1)),
    <<"bytes ", StartOffsetBin/binary, "-", EndOffsetBin/binary, "/", BodySizeBin/binary>>.

range_not_satisfiable_resp(BodySize) ->
    BodySizeBin = list_to_binary(integer_to_list(BodySize)),
    {416, [{<<"content-range">>, <<"bytes */", BodySizeBin/binary>>}], <<"">>}.

handle_conditions(Request, #{<<"if-none-match">> := '*'}, Response, _RespMetadata) ->
    handle_failed_condition(Request, Response);
handle_conditions(Request,
                  #{<<"if-none-match">> := IfNoneMatch},
                  Response,
                  #{parsed_headers := #{<<"etag">> := {_StrongOrWeak, EtagVal}}}) ->
    {_, IfNoneMatchVals} = lists:unzip(IfNoneMatch),
    case lists:member(EtagVal, IfNoneMatchVals) of
        true ->
            handle_failed_condition(Request, Response);
        false ->
            Response
    end;
handle_conditions(_Request, #{<<"if-none-match">> := _}, Response, _RespMetadata) ->
    Response;
handle_conditions(Request,
                  #{<<"if-modified-since">> := IfModifiedSince},
                  Response,
                  #{parsed_headers := #{<<"last-modified">> := LastModified}})
    when LastModified =< IfModifiedSince ->
    handle_failed_condition(Request, Response);
handle_conditions(Request,
                  #{<<"if-modified-since">> := IfModifiedSince},
                  Response,
                  #{parsed_headers := #{<<"date">> := Date}})
    when Date =< IfModifiedSince ->
    handle_failed_condition(Request, Response);
handle_conditions(Request,
                  #{<<"if-modified-since">> := IfModifiedSince},
                  Response,
                  #{created := Created})
    when Created =< IfModifiedSince ->
    handle_failed_condition(Request, Response);
handle_conditions(_Request, _ParsedReqHeaders, Response, _RespMetadata) ->
    Response.

handle_failed_condition({Method, _Url, _ReqHeaders, _ReqBody},
                        {_Status, RespHeaders, _RespBody})
    when Method == <<"HEAD">> orelse Method == <<"GET">> ->
    {304,
     keep_headers(RespHeaders,
                  [<<"cache-control">>,
                   <<"content-location">>,
                   <<"date">>,
                   <<"etag">>,
                   <<"expires">>,
                   <<"vary">>,
                   <<"last-modified">>]),
     <<>>};
handle_failed_condition(_Request, _Response) ->
    {412, [], <<>>}.

parse_headers(Headers, Which) ->
    parse_headers(Headers, maps:from_keys(Which, []), #{}).

parse_headers([], _Which, Acc) ->
    Acc;
parse_headers([{HeaderName, HeaderValue} | Rest], Which, Acc) ->
    LCHeaderName = ?LOWER(HeaderName),
    case is_map_key(LCHeaderName, Which) of
        true ->
            ParsedValue = parse_header_value(LCHeaderName, HeaderValue),
            case {ParsedValue, maps:get(LCHeaderName, Acc, undefined)} of
                {#{} = NewVal, #{} = CurrentVal} ->
                    parse_headers(Rest,
                                  Which,
                                  maps:put(LCHeaderName, maps:merge(CurrentVal, NewVal), Acc));
                {[_ | _] = NewVal, [_ | _] = CurrentVal} ->
                    parse_headers(Rest, Which, maps:put(LCHeaderName, CurrentVal ++ NewVal, Acc));
                {Val, _} when Val /= undefined ->
                    parse_headers(Rest, Which, maps:put(LCHeaderName, Val, Acc));
                _ ->
                    parse_headers(Rest, Which, Acc)
            end;
        false ->
            parse_headers(Rest, Which, Acc)
    end.

% cowlib supports only no value or "no-cache"
parse_header_value(<<"pragma">>, HeaderValue) ->
    case binary:match(HeaderValue, <<"no-cache">>) of
        nomatch ->
            cache;
        _ ->
            no_cache
    end;
% cowlib will not parse unknown schemes
parse_header_value(<<"authorization">>, HeaderValue) ->
    HeaderValue;
parse_header_value(HeaderName, HeaderValue) ->
    HeaderNameUnderscore = binary:replace(HeaderName, <<"-">>, <<"_">>, [global]),
    FunName = erlang:binary_to_atom(<<"parse_", HeaderNameUnderscore/binary>>),
    try
        post_process_header_parsing(HeaderName, apply(cow_http_hd, FunName, [HeaderValue]))
    catch
        _:_ ->
            undefined
    end.

post_process_header_parsing(HeaderName, ParsedValue)
    when is_map_key(HeaderName, ?KV_HEADERS) ->
    lists:foldl(fun ({Name, Val}, Acc) ->
                        maps:put(Name, Val, Acc);
                    (Name, Acc) ->
                        maps:put(Name, undefined, Acc)
                end,
                #{},
                ParsedValue);
post_process_header_parsing(_, {{_, _, _}, {_, _, _}} = ErlangDateTime) ->
    calendar:datetime_to_gregorian_seconds(ErlangDateTime) - 62167219200;
post_process_header_parsing(_, Val) ->
    Val.

set_header_value(HeaderName, Value, Headers) when is_map_key(HeaderName, ?CSL_HEADERS) ->
    set_csl_header_value(HeaderName, Value, Headers, []);
set_header_value(HeaderName, Value, Headers) ->
    delete_header(HeaderName, Headers) ++ [{HeaderName, Value}].

set_csl_header_value(TargetHeader, Value, [], Acc) ->
    lists:reverse([{TargetHeader, Value} | Acc]);
set_csl_header_value(TargetHeader, Value, [{HeaderName, HeaderValue} | Rest], Acc) ->
    case ?LOWER(HeaderName) of
        TargetHeader ->
            lists:reverse(Acc)
            ++ [{HeaderName, <<HeaderValue/binary, ", ", Value/binary>>}]
            ++ Rest;
        _ ->
            set_csl_header_value(TargetHeader, Value, Rest, [{HeaderName, HeaderValue} | Acc])
    end.

keep_headers(Headers, Which) ->
    keep_headers(Headers, maps:from_keys(Which, undefined), []).

keep_headers([], _Which, Acc) ->
    lists:reverse(Acc);
keep_headers([{HeaderName, HeaderValue} | Rest], Which, Acc) ->
    case is_map_key(?LOWER(HeaderName), Which) of
        true ->
            keep_headers(Rest, Which, [{HeaderName, HeaderValue} | Acc]);
        false ->
            keep_headers(Rest, Which, Acc)
    end.

delete_header(ToDelete, Headers) ->
    delete_header(ToDelete, Headers, []).

delete_header(_ToDelete, [], Acc) ->
    lists:reverse(Acc);
delete_header(ToDelete, [{HeaderName, HeaderValue} | Rest], Acc) ->
    case ?LOWER(HeaderName) of
        ToDelete ->
            delete_header(ToDelete, Rest, Acc);
        _ ->
            delete_header(ToDelete, Rest, [{HeaderName, HeaderValue} | Acc])
    end.

parsed_content_type_to_string({MainType, SubType, Params}) ->
    JoinedParams =
        iolist_to_binary([<<"; ", Name/binary, "=", Value/binary>> || {Name, Value} <- Params]),
    <<MainType/binary, "/", SubType/binary, JoinedParams/binary>>.

log_invalidation_result({ok, NbInvalidation}, Type, InvalidationDur)
    when is_integer(NbInvalidation) ->
    telemetry:execute(?TELEMETRY_INVALIDATION_EVT,
                      #{count => NbInvalidation, duration => InvalidationDur},
                      #{type => Type}),
    {ok, NbInvalidation};
log_invalidation_result({ok, undefined}, Type, InvalidationDur) ->
    telemetry:execute(?TELEMETRY_INVALIDATION_EVT,
                      #{count => 1, duration => InvalidationDur},
                      #{type => Type}),
    {ok, undefined};
log_invalidation_result(Error, _Type, _InvlidationDur) ->
    Error.

unix_now() ->
    os:system_time(second).

now_monotonic_us() ->
    erlang:monotonic_time(microsecond).
