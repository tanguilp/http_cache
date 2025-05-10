%%%-----------------------------------------------------------------------------
%%% @doc An HTTP caching library
%%%
%%% `http_cache' is a stateless Erlang HTTP caching library that implements the various
%%% HTTP RFCs related to caching.
%%%
%%% == Modules ==
%%%
%%% {@link http_cache} exposes functions to cache backend responses, get cached responses
%%% whenever they can be served, and invalidate previously stored responses.
%%%
%%% {@link http_cache_store} is the behaviour to be implemented by stores.
%%%
%%% {@link http_cache_store_process} is an example store that stores cached responses in
%%% the current process and is mainly used for testing purpose.
%%%
%%% == Telemetry events ==
%%%
%%% All time measurements are in microseconds.
%%%
%%% The following events are emitted by `http_cache':
%%% <ul>
%%%   <li>
%%%     `[http_cache, lookup]' when {@link http_cache:get/2} is called.
%%%
%%%     Measurements:
%%%     <ul>
%%%       <li>`total_time': the total time of the lookup</li>
%%%       <li>`store_lookup_time': time taken to query the store for suitable responses</li>
%%%       <li>`response_selection_time': time to select the best response among suitable
%%%       responses. A high value can indicate the presence of too many variants</li>
%%%       <li>`candidate_count': the number of candidate responses that are returned by the
%%%       store. A high value can indicate the presence of too many variants</li>
%%%       <li>`decompress_time': time spend decompressing the response</li>
%%%       <li>`range_time': time spend constructing a range response</li>
%%%     </ul>
%%%
%%%     Metadata:
%%%     <ul>
%%%       <li>`freshness': one of `fresh', `stale', `must_revalidate' or `miss'</li>
%%%     </ul>
%%%   </li>
%%%   <li>
%%%     `[http_cache, cache]' when {@link http_cache:cache/3} or {@link http_cache:cache/4}
%%%     is called.
%%%
%%%     Measurements:
%%%     <ul>
%%%       <li>`total_time': the total time of the caching operation</li>
%%%       <li>`store_save_time': time taken to save the response into the store</li>
%%%       <li>`compress_time': time spend compressing the response. This happens when the
%%%       `auto_compress' option is used</li>
%%%       <li>`decompress_time': time spend decompressing the response. This happens when the
%%%       `auto_compress' option is used but the client does not support compression and
%%%       the result, stored compressed, has to be returned uncompressed</li>
%%%       <li>`range_time': time spend constructing a range response</li>
%%%     </ul>
%%%
%%%     Metadata:
%%%     <ul>
%%%       <li>`cacheable': `true' if the response was cacheable (and cached), `false' otherwise</li>
%%%     </ul>
%%%   </li>
%%%   <li>
%%%     `[http_cache, invalidation]' when {@link http_cache:invalidate_url/2} or
%%%     {@link http_cache:invalidate_by_alternate_key/2} is called.
%%%
%%%     Measurements:
%%%     <ul>
%%%       <li>`duration': the time it took to invalidate entries</li>
%%%       <li>
%%%         `count': the number of entries invalidated if the store supports returning this value
%%%       </li>
%%%     </ul>
%%%
%%%     Metadata:
%%%     <ul>
%%%       <li>`type': `invalidate_by_url' or `invalidate_by_alternate_key'</li>
%%%     </ul>
%%%   </li>
%%%   <li>
%%%     `[http_cache, store, error]' informs about errors of the store.
%%%
%%%     Measurements: none
%%%
%%%     Metadata:
%%%     <ul>
%%%       <li>`type': `cache', `invalidate_by_url` or `invalidate_by_alternate_key'</li>
%%%       <li>`reason': an erlang term that gives the error reason</li>
%%%     </ul>
%%%   </li>
%%%   <li>
%%%     `[http_cache, compress_operation]' whenever a compress operation is performed on an
%%%     HTTP response.
%%%
%%%     Measurements: none
%%%
%%%     Metadata:
%%%     <ul>
%%%       <li>`alg': `gzip' (which is the only supported algorithm at the moment)</li>
%%%     </ul>
%%%   </li>
%%%   <li>
%%%     `[http_cache, decompress_operation]' whenever a decompress operation is performed on an
%%%     HTTP response.
%%%
%%%     Measurements: none
%%%
%%%     Metadata:
%%%     <ul>
%%%       <li>`alg': `gzip' (which is the only supported algorithm at the moment)</li>
%%%     </ul>
%%%   </li>
%%% </ul>
%%% @end
%%%-----------------------------------------------------------------------------
-module(http_cache).

%% API exports
-export([get/2, notify_downloading/3, notify_response_used/2, cache/3, cache/4,
         invalidate_url/2, invalidate_by_alternate_key/2]).

-export_type([alternate_key/0, headers/0, request/0, response/0, status/0, timestamp/0]).

-include_lib("cowlib/include/cow_inline.hrl").
-include_lib("kernel/include/file.hrl").

-type alternate_key() :: term().
%% Alternate key attached to a stored response
%%
%% Used to invalidate by alternate key (e.g. to invalidate all the images if
%% the `image' alternate key if set to all images).
-type body() :: iodata().
%% Request or response body
-type headers() :: [{binary(), binary()}].
%% Request or response headers
%%
%% A header can appear more than once, this is allowed by HTTP
-type invalidation_result() ::
    {ok, NbInvalidatedResponses :: non_neg_integer() | undefined} | {error, term()}.

%% The invalidation result, with the count of deleted objects if the backend supports it
%%
%% `undefined' is returned if the backend does not support counting the number of
%% invalidated responses.
%%
%% `{error, term()}' is returned by the backend in case of error.

-type method() :: binary().
%% An HTTP method, for example "PATCH"
-type opts() ::
    #{store := module(),
      alternate_keys => [alternate_key()],
      allow_stale_while_revalidate => boolean(),
      auto_accept_encoding => boolean(),
      auto_compress => boolean(),
      auto_decompress => boolean(),
      bucket => term(),
      compression_threshold => non_neg_integer(),
      default_ttl => non_neg_integer(),
      default_grace => non_neg_integer(),
      ignore_query_params_order => boolean(),
      max_ranges => non_neg_integer(),
      prevent_set_cookie => auto | boolean(),
      request_time => non_neg_integer(),
      store_opts => http_cache_store_behaviour:opts(),
      type => type(),
      atom() => any()}.
%% Options passed to the functions of this module
%%
%% <ul>
%%  <li>
%%    `alternate_keys': alternate keys associated with the stored request. Requests
%%    can then be invalidated by alternate key with {@link invalidate_by_alternate_key/2}.
%%    Use by {@link cache/3} and {@link cache/4}.
%%  </li>
%%  <li>
%%    `allow_stale_while_revalidate': allows returning valid stale response while revalidating.
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
%%    Used by {@link cache/3} and {@link cache/4}.  Defaults to `false'.
%%  </li>
%%  <li>
%%    `auto_compress_mime_types': the list of mime-types that are compressed when
%%    `auto_compress' is used.
%%
%%    Used by {@link cache/3} and {@link cache/4}.  Defaults to
%%    `[<<"text/html">>, <<"text/css">>, <<"text/plain">>, <<"text/xml">>, <<"text/javascript">>, <<"application/javascript">>, <<"application/json">>, <<"application/ld+json">>, <<"application/xml">>, <<"application/xhtml+xml">>, <<"application/rss+xml">>, <<"application/atom+xml">>, <<"image/svg+xml">>, <<"font/ttf">>, <<"font/eot">>, <<"font/otf">>, <<"font/opentype">> ]'
%%  </li>
%%  <li>
%%    `auto_decompress': automatically decompresses stored gzip responses when the client
%%    does not support compression.
%%
%%    Does not decompress responses with strong etags (see
%%    [https://bz.apache.org/bugzilla/show_bug.cgi?id=63932]).
%%
%%    Used by {@link get/2}, {@link cache/3} and {@link cache/4}.  Defaults to `false'.
%%  </li>
%%  <li>
%%    `bucket': an Erlang term to differentiate between different caches. For instance,
%%    when what needs to use several private caches, this option can be used to differentiate
%%    the cached responses and prevent them from being mixed up, potentially leaking
%%    private data.
%%    Used by {@link get/2}, {@link cache/3} and {@link cache/4}. Defaults to the atom `default'.
%%  </li>
%%  <li>
%%    `compression_threshold': compression threshold in bytes. Compressing a very tiny
%%    response can result in actually bigger response (in addition to the performance
%%    hit of compression it).
%%
%%    Although there's no additional cost when this library serves a
%%    compressed file, but it has a cost on the client that has to decompress it.
%%
%%    This is why the default value is so high: we want to make sure that it's worth performing
%%    compression and decompression.
%%
%%    See further discussion:
%%    [https://webmasters.stackexchange.com/questions/31750/what-is-recommended-minimum-object-size-for-gzip-performance-benefits].
%%
%%    Used by {@link cache/3} and {@link cache/4}. Defaults to `1000'.
%%  </li>
%%  <li>
%%    `default_ttl': the default TTL, in seconds. This value is used when no TTL information
%%    is found in the response, but the response is cacheable by default (see
%%    [https://datatracker.ietf.org/doc/html/rfc7231#section-6.1]).
%%    Used by {@link cache/3} and {@link cache/4}. Defaults to `120'.
%%  </li>
%%  <li>
%%    `default_grace': the amount of time an expired response is kept in the cache. Such
%%    a response is called a stale response, and can be returned in some circumstances, for
%%    instance when the origin server returns an `5xx' error and `stale-if-error' header is used.
%%    Use by {@link cache/3} and {@link cache/4}. Defaults to `120'.
%%  </li>
%%  <li>
%%    `ignore_query_params_order': when a response is cached, a request key is computed based
%%    on the method, URL and body. This option allows to keep the same request key for URLs
%%    whose parameters are identical, but in different order. This helps increasing cache hit if
%%    URL parameter order doesn't matter.
%%    Used by {@link get/2}, {@link cache/3} and {@link cache/4}. Defaults to `false'.
%%  </li>
%%  <li>
%%    `max_ranges': maximum number of range sets accepted when responding to a range request.
%%    This is limited to avoid DOS attack by a client.
%%    See [https://datatracker.ietf.org/doc/html/rfc7233#section-6.1].
%%    Used by {@link get/2}. Defaults to `100'.
%%  </li>
%%  <li>
%%    `prevent_set_cookie': when set to `auto', raises when the `set-cookie' is used on shared
%%    caches. When set to `true', always raises in this case. When set to `false', never raises
%%    even for shared caches.
%%    Used by {@link cache/3} and {@link cache/4}. Defaults to `auto'.
%%  </li>
%%  <li>
%%    `store': <b>required</b>, the store backend's module name.
%%    Used by all functions, no defaults.
%%  </li>
%%  <li>
%%    `store_opts': the store backend's options.
%%    Used by all functions, defaults to `[]'.
%%  </li>
%%  <li>
%%    `type': cache type. `shared' or `private'. A CDN is an example of a shared cache. A browser cache
%%    is an example of a private cache.
%%    Used by {@link get/2}, {@link cache/3} and {@link cache/4}. Defaults to `shared'.
%%  </li>
%%  <li>
%%    `request_time': the time the request was initiated, as a UNIX timestamp in seconds.
%%    Setting this timestamp helps correcting the age of the request between the time the request
%%    was made and the time the response was received and cached, which can be several seconds.
%%    Used by {@link cache/3} and {@link cache/4}.
%%  </li>
%% </ul>
-type request() :: {method(), url(), headers(), body()}.
%% An HTTP request
-type response() :: {status(), headers(), body() | sendfile()}.
%% An HTTP response
-type url() :: binary().
%% An URL, with the schema, domain and optionally path
-type status() :: pos_integer().
%% HTTP status
-type sendfile() ::
    {sendfile,
     Offset :: non_neg_integer(),
     Length :: non_neg_integer() | all,
     Path :: binary()}.
-type timestamp() :: non_neg_integer().
%% UNIX timestamp in seconds
-type type() :: shared | private.

%% Type of the cache: shared (like proxies) or private (like browser cache)

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
          <<"vary">> => []}).
-define(KV_HEADERS, #{<<"accept-encoding">> => [], <<"cache-control">> => []}).
-define(RESP_HEADERS_TO_DELETE,
        #{<<"connection">> => [],
          <<"proxy-authenticate">> => [],
          <<"proxy-connection">> => [],
          <<"keep-alive">> => [],
          <<"transfer-encoding">> => [],
          <<"upgrade">> => []}).
-define(DEFAULT_OPTS,
        #{allow_stale_while_revalidate => false,
          backend_in_error => false,
          alternate_keys => [],
          auto_accept_encoding => false,
          auto_compress => false,
          auto_decompress => false,
          compression_threshold => 1000,
          bucket => default,
          default_ttl => ?DEFAULT_TTL,
          default_grace => ?DEFAULT_GRACE,
          ignore_query_params_order => false,
          max_ranges => 100,
          prevent_set_cookie => auto,
          store_opts => [],
          type => shared}).
-define(PDICT_MEASUREMENTS, http_cache_measurments).
-define(TELEMETRY_LOOKUP_EVT, [http_cache, lookup]).
-define(TELEMETRY_CACHE_EVT, [http_cache, cache]).
-define(TELEMETRY_INVALIDATION_EVT, [http_cache, invalidation]).
-define(TELEMETRY_COMPRESS_EVT, [http_cache, compress_operation]).
-define(TELEMETRY_DECOMPRESS_EVT, [http_cache, decompress_operation]).
-define(TELEMETRY_STORE_ERROR_EVT, [http_cache, store, error]).
-define(TELEMETRY_PROCESS_TAG, {?MODULE, telemetry_data}).
-define(is_no_transform(Headers),
        (is_map_key(<<"cache-control">>, Headers)
        andalso is_map_key(<<"no-transform">>, map_get(<<"cache-control">>, Headers)))).
-define(has_strong_etag(ParsedRespHeaders),
        (is_map_key(<<"etag">>, ParsedRespHeaders)
        andalso element(1, map_get(<<"etag">>, ParsedRespHeaders)) == strong)).

%%====================================================================
%% API functions
%%====================================================================

%%------------------------------------------------------------------------------
%% @doc Gets a response from the cache for the given answer
%%
%% The function returns one of:
%% <ul>
%%    <li>`{fresh, {http_cache_store_behaviour:response_ref(), response()}}':
%%    the response is fresh and can be returned directly to the client.
%%    </li>
%%    <li>`{stale, {http_cache_store_behaviour:response_ref(), response()}}':
%%    the response is stale but can be directly returned to the client.
%%
%%    A stale response can be returned when:
%%      <ul>
%%        <li>the request `max-stale' directive is used by the client</li>
%%        <li>the `allow-stale-while-revalidate' directive is used either by the client or the
%%        server</li>
%%        <li>the `stale-if-error' is used. However such stale responses are returned by
%%        {@link cache/3} and {@link cache/4} and not by this function</li>
%%      </ul>
%%    </li>
%%    <li>`{must_revalidate, {http_cache_store_behaviour:response_ref(), response()}}':
%%    the response must be revalidated.
%%    </li>
%%    <li>`miss': no suitable response was found. </li>
%% </ul>
%%
%% Using this function does not automatically notify the response was returned.
%% Therefore, use {@link notify_response_used/2} with the returned response
%% reference when a cached response is used.
%%
%% @end
%%------------------------------------------------------------------------------

-spec get(request(), opts()) ->
             {fresh, {http_cache_store_behaviour:response_ref(), response()}} |
             {stale, {http_cache_store_behaviour:response_ref(), response()}} |
             {must_revalidate, {http_cache_store_behaviour:response_ref(), response()}} |
             miss.
get(Request, Opts) ->
    try
        telemetry_start_measurement(total_time),
        Result = do_get(Request, init_opts(Opts)),
        telemetry_stop_measurement(total_time),
        telemetry_send_event(?TELEMETRY_LOOKUP_EVT),
        Result
    catch
        resp_body_no_longer_available ->
            % The response was deleted from the backend between analysis time and fetch time.
            % This might be because a more up to date response was uploaded into the cache.
            % We retry 3 times before returning miss
            case get(http_cache_get_attempt_count) of
                undefined ->
                    put(http_cache_get_attempt_count, 1),
                    get(Request, Opts);
                NbAttempts when NbAttempts =< 3 ->
                    put(http_cache_get_attempt_count, NbAttempts + 1),
                    get(Request, Opts);
                _ ->
                    miss
            end
    end.

do_get({_Method, _Url, ReqHeaders, _ReqBody} = Request,
       #{store := Store, store_opts := StoreOpts} = Opts) ->
    RequestKey = request_key(Request, Opts),
    telemetry_start_measurement(store_lookup_time),
    Candidates = Store:list_candidates(RequestKey, StoreOpts),
    telemetry_stop_measurement(store_lookup_time),
    telemetry_set_measurement(candidate_count, length(Candidates)),
    ParsedReqHeaders =
        parse_headers(ReqHeaders,
                      [<<"cache-control">>,
                       <<"pragma">>,
                       <<"accept-encoding">>,
                       <<"range">>,
                       <<"if-none-match">>,
                       <<"if-modified-since">>,
                       <<"if-range">>]),
    telemetry_start_measurement(response_selection_time),
    MaybeCandidate =
        select_candidate(ReqHeaders, ParsedReqHeaders, Candidates, undefined, Opts),
    telemetry_stop_measurement(response_selection_time),
    case MaybeCandidate of
        {Freshness, {RespRef, _Status, _RespHeaders, _VaryHeaders, _RespMetadata}} ->
            telemetry_set_metadata(freshness, Freshness),
            telemetry_start_measurement(store_get_response_time),
            MaybeResponse = Store:get_response(RespRef, StoreOpts),
            telemetry_stop_measurement(store_get_response_time),
            case MaybeResponse of
                {Status, RespHeaders, RespBody, RespMetadata} ->
                    postprocess_response(Freshness,
                                         Request,
                                         ParsedReqHeaders,
                                         RespRef,
                                         {Status, RespHeaders, RespBody},
                                         RespMetadata,
                                         Opts);
                undefined ->
                    throw(resp_body_no_longer_available)
            end;
        undefined ->
            telemetry_set_metadata(freshness, miss),
            postprocess_response(miss,
                                 Request,
                                 ParsedReqHeaders,
                                 undefined,
                                 undefined,
                                 undefined,
                                 Opts)
    end.

postprocess_response(Freshness,
                     _Request,
                     #{<<"cache-control">> := #{<<"only-if-cached">> := _}},
                     _MaybeRespRef,
                     _MaybeResponse,
                     _RespMetadata,
                     _Opts)
    when Freshness == must_revalidate orelse Freshness == miss ->
    {fresh, {undefined, {504, [], <<"">>}}};
postprocess_response(miss,
                     _Request,
                     _ParsedReqHeaders,
                     _RespRef,
                     _Response0,
                     _RespMetadata,
                     _Opts) ->
    miss;
postprocess_response(Freshness,
                     Request,
                     ParsedReqHeaders,
                     RespRef,
                     Response0,
                     RespMetadata,
                     Opts) ->
    case handle_conditions(Request, ParsedReqHeaders, Response0, RespMetadata) of
        {304, _, _} = Response1 ->
            {Freshness, {RespRef, Response1}};
        Response1 ->
            Response2 =
                transform_response(Request, ParsedReqHeaders, Response1, RespMetadata, Opts),
            Response3 = set_age_header(Response2, RespMetadata),
            Response4 = handle_file_body(Response3),
            {Freshness, {RespRef, Response4}}
    end.

transform_response(Request, ParsedReqHeaders, Response0, RespMetadata, Opts) ->
    telemetry_start_measurement(decompress_time),
    Response1 = handle_auto_decompress(ParsedReqHeaders, Response0, RespMetadata, Opts),
    telemetry_stop_measurement(decompress_time),
    telemetry_start_measurement(range_time),
    Response2 =
        handle_range_request(Request, ParsedReqHeaders, Response1, RespMetadata, Opts),
    telemetry_stop_measurement(range_time),
    Response2.

%%------------------------------------------------------------------------------
%% @doc Notifies a response is currently being downloaded
%%
%% For future use, does not do anything at the moment.
%% @end
%%------------------------------------------------------------------------------

-spec notify_downloading(request(), pid(), opts()) -> ok.
notify_downloading(_Request, _Pid, _Opts) ->
    ok.

%%------------------------------------------------------------------------------
%% @doc Notifies the backend that a response was used
%%
%% Some backends, such as LRU backends, need to update metadata (in that case:
%% last used time) when a response is used.
%% @end
%%------------------------------------------------------------------------------
-spec notify_response_used(http_cache_store_behaviour:response_ref(), opts()) ->
                              ok | {error, term()}.
notify_response_used(RespRef, Opts) ->
    #{store := Store, store_opts := StoreOpts} = init_opts(Opts),
    Store:notify_response_used(RespRef, StoreOpts).

%%------------------------------------------------------------------------------
%% @doc Caches a response
%%
%% This function never returns an error, even when the backend store returns one.
%% Instead it returns `{ok, response()}' when the response is cacheable (even
%% if an error occurs to actually save it) or `not_cacheable' when the response
%% cannot be cached.
%%
%% When `{ok, response()}' is returned, the response should be returned to the client
%% instead of the initial response that was passed as a parameter, because it is
%% transformed accordingly to the options passed: it can be compressed or uncompressed,
%% and it will be returned as a range response if the request is a range
%% request and the backend doesn't support it and returned a full response.
%%
%% `{not_cacheable, response()}' is returned when the response in parameters:
%% <ul>
%%   <li>has a `500', `502', `503' or `504' error status</li>
%%   <li>is not cacheable (e.g. it has no explicit `max-age' cache-control directive)</li>
%%   <li>a response observing the `stale-if-error' cache control directive has been found</li>
%% </ul>
%%
%% This function shall be called with any response, even those known to be not
%% cacheable, such as `DELETE' requests, because such non-cacheable request
%% can still have side effects on other cached objects
%% (see [https://www.rfc-editor.org/rfc/rfc9111.html#name-invalidating-stored-respons]).
%% In this example, a successful `DELETE' request triggers the
%% invalidation of cached results of the deleted object with the same URL.
%%
%% @end
%%------------------------------------------------------------------------------

-spec cache(request(), response(), opts()) ->
               {ok, response()} | {not_cacheable, response()} | not_cacheable.
cache(Request, {_, RespHeaders, _} = Response, Opts) ->
    telemetry_start_measurement(total_time),
    NormOpts = init_opts(Opts),
    check_set_cookie(RespHeaders, NormOpts),
    Result = do_cache(Request, Response, NormOpts),
    telemetry_stop_measurement(total_time),
    telemetry_send_event(?TELEMETRY_CACHE_EVT),
    Result.

do_cache({<<"HEAD">>, _, _, _} = Request, {200, _, _} = Response, NormOpts) ->
    refresh_stored_responses(Request, Response, NormOpts),
    analyze_cache(Request, Response, NormOpts);
do_cache(Request, {304, _, _} = Response, NormOpts) ->
    refresh_stored_responses(Request, Response, NormOpts),
    {ok, Response};
do_cache(Request, Response, NormOpts) ->
    analyze_cache(Request, Response, NormOpts).

%%------------------------------------------------------------------------------
%% @doc Caches a response when revalidating
%%
%% Similar to {@link cache/3}, but to be used when revalidating a response,
%% when {@link get/2} return a `:must_revalidate' response. The `Response'
%% parameter is the response received from the origin server, and the
%% `RevalidatedResponse' parameter is the previously `:must_revalidate' response
%% that is being revalidated.
%%
%% When the returned response is a `304' (not modified) response, stored
%% responses are updated and a response is returned from the 2 responses passed
%% as a parameter. It's recommended to use the response returned by this function,
%% because the `304' response is used to update headers of the revalidated response.
%%
%% Otherwise, {@link cache/3} is called.
%%
%% @end
%%------------------------------------------------------------------------------

-spec cache(Request :: request(),
            Response :: response(),
            RevalidatedResponse :: response(),
            opts()) ->
               {ok, response()} | {not_cacheable, response()} | not_cacheable.
cache(Request, Response, RevalidatedResponse, Opts) ->
    telemetry_start_measurement(total_time),
    Result = do_cache(Request, Response, RevalidatedResponse, init_opts(Opts)),
    telemetry_stop_measurement(total_time),
    telemetry_send_event(?TELEMETRY_CACHE_EVT),
    Result.

do_cache(Request,
         {304, RespHeaders, _} = Response,
         {Status, RevalidatedRespHeaders, RespBody},
         NormOpts) ->
    check_set_cookie(RespHeaders, NormOpts),
    refresh_stored_responses(Request, Response, NormOpts),
    UpdatedHeaders = update_headers(RevalidatedRespHeaders, RespHeaders),
    {ok, {Status, UpdatedHeaders, RespBody}};
do_cache(Request, Response, _, NormOpts) ->
    do_cache(Request, Response, NormOpts).

analyze_cache({Method, _Url, ReqHeaders, _ReqBody} = Request,
              {Status, RespHeaders, _RespBody} = Response,
              Opts) ->
    telemetry_start_measurement(analysis_time),
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
    % A POST request that triggers invalidation cannot be cacheable
    case must_invalidate_request_uri(Request, Response, ParsedRespHeaders) of
        true ->
            handle_invalidation_of_unsafe_method(Request, RespHeaders, Opts),
            telemetry_set_metadata(cacheable, false),
            not_cacheable;
        false ->
            ParsedReqHeaders =
                parse_headers(ReqHeaders,
                              [<<"authorization">>,
                               <<"cache-control">>,
                               <<"pragma">>,
                               <<"range">>]),

            case is_cacheable(Method, ParsedReqHeaders, Status, ParsedRespHeaders, Opts) of
                true ->
                    telemetry_set_metadata(cacheable, true),
                    do_cache(Request, ParsedReqHeaders, Response, ParsedRespHeaders, Opts);
                false ->
                    telemetry_set_metadata(cacheable, false),
                    handle_stale_if_error(Request, Response, Opts)
            end
    end.

handle_stale_if_error(Request, {Status, _, _}, Opts)
    when Status == 500; Status == 502; Status == 503; Status == 504 ->
    case get(Request, Opts#{backend_in_error => true}) of
        {fresh, {_RespRef, Response}} ->
            {not_cacheable, Response};
        {stale, {_RespRef, Response}} ->
            {not_cacheable, Response};
        _ ->
            not_cacheable
    end;
handle_stale_if_error(_, _, _) ->
    not_cacheable.

do_cache({Method, Url, ReqHeaders0, ReqBody} = Request,
         ParsedReqHeaders0,
         {Status, RespHeaders0, RespBody},
         #{<<"content-type">> := {MainType, SubType, _}} = ParsedRespHeaders0,
         #{auto_compress := true, compression_threshold := CompressionThresold} = Opts)
    when byte_size(RespBody) >= CompressionThresold,
         not is_map_key(<<"content-encoding">>, ParsedRespHeaders0),
         not ?is_no_transform(ParsedRespHeaders0),
         is_map_key({MainType, SubType}, ?DEFAULT_COMPRESS_MIME_TYPES),
         not ?has_strong_etag(ParsedRespHeaders0) ->
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
    telemetry_start_measurement(compress_time),
    GzippedBody = zlib:gzip(RespBody),
    telemetry_stop_measurement(compress_time),
    telemetry:execute(?TELEMETRY_COMPRESS_EVT, #{}, #{alg => gzip}),
    {ok, Response} =
        do_cache({Method, Url, ReqHeaders, ReqBody},
                 ParsedReqHeaders,
                 {Status, RespHeaders, GzippedBody},
                 ParsedRespHeaders,
                 Opts),
    RespMetadata = response_metadata(ParsedRespHeaders, Opts),
    ParsedReqHeadersFinal =
        maps:merge(ParsedReqHeaders0, parse_headers(ReqHeaders0, [<<"accept-encoding">>])),
    TransformedResponse =
        transform_response(Request, ParsedReqHeadersFinal, Response, RespMetadata, Opts),
    {ok, TransformedResponse};
do_cache({_Method, Url, ReqHeaders0, _ReqBody} = Request,
         ParsedReqHeaders,
         {Status, RespHeaders0, RespBody0},
         ParsedRespHeaders,
         #{store := Store, store_opts := StoreOpts} = Opts) ->
    RespBodyBin = iolist_to_binary(RespBody0),
    % the response doesn't necessarily has the content length set, but we have this
    % information, so let's be a good citizen of the web and always set it
    RespHeaders1 =
        set_header_value(<<"content-length">>,
                         list_to_binary(integer_to_list(byte_size(RespBodyBin))),
                         RespHeaders0),
    RespHeaders2 = strip_connection_headers(RespHeaders1),
    RequestKey = request_key(Request, Opts),
    VaryHeaders = vary_headers(ReqHeaders0, ParsedRespHeaders),
    UrlDigest = url_digest(Url, Opts),
    Response = {Status, RespHeaders2, RespBodyBin},
    RespMetadata = response_metadata(ParsedRespHeaders, Opts),
    telemetry_stop_measurement(analysis_time),
    telemetry_start_measurement(store_save_time),
    StoreRes =
        Store:put(RequestKey, UrlDigest, VaryHeaders, Response, RespMetadata, StoreOpts),
    telemetry_stop_measurement(store_save_time),
    telemetry_log_cache_store_result(StoreRes),
    TransformedResponse =
        transform_response(Request, ParsedReqHeaders, Response, RespMetadata, Opts),
    {ok, TransformedResponse}.

response_metadata(ParsedRespHeaders, Opts) ->
    MaybeAge = maps:get(<<"age">>, ParsedRespHeaders, undefined),
    MaybeDate = maps:get(<<"date">>, ParsedRespHeaders, undefined),
    CreatedAt = created_at(MaybeAge, MaybeDate, Opts),
    {TTLSetBy, Expires} = expires(CreatedAt, MaybeDate, ParsedRespHeaders, Opts),
    #{created => CreatedAt,
      expires => Expires,
      grace => grace(Expires, Opts),
      ttl_set_by => TTLSetBy,
      % For more efficiency, we store only headers we might need when getting responses
      parsed_headers =>
          maps:with([<<"cache-control">>,
                     <<"content-encoding">>,
                     <<"content-type">>,
                     <<"date">>,
                     <<"etag">>,
                     <<"last-modified">>,
                     <<"pragma">>],
                    ParsedRespHeaders),
      alternate_keys => map_get(alternate_keys, Opts)}.

%%------------------------------------------------------------------------------
%% @doc Invalidates all responses for a URL
%%
%% This includes all variants and all responses for all HTTP methods.
%% @end
%%------------------------------------------------------------------------------
-spec invalidate_url(url(), opts()) -> invalidation_result().
invalidate_url(Url, Opts) ->
    do_invalidate_url(Url, init_opts(Opts)).

do_invalidate_url(Url, #{store := Store, store_opts := StoreOpts} = Opts) ->
    UrlDigest = url_digest(Url, Opts),
    {InvalidationDur, InvalidationRes} =
        timer:tc(Store, invalidate_url, [UrlDigest, StoreOpts]),
    telemetry_log_invalidation_result(InvalidationRes, invalidate_by_url, InvalidationDur).

%%------------------------------------------------------------------------------
%% @doc Invalidates all responses stored with the alternate key
%% @end
%%------------------------------------------------------------------------------
-spec invalidate_by_alternate_key(alternate_key() | [alternate_key()], opts()) ->
                                     invalidation_result().
invalidate_by_alternate_key(AltKeys, Opts) ->
    do_invalidate_by_alternate_key(AltKeys, init_opts(Opts)).

do_invalidate_by_alternate_key([], _Opts) ->
    {ok, 0};
do_invalidate_by_alternate_key([_ | _] = AltKeys,
                               #{store := Store, store_opts := StoreOpts}) ->
    {InvalidationDur, InvalidationRes} =
        timer:tc(Store, invalidate_by_alternate_key, [AltKeys, StoreOpts]),
    telemetry_log_invalidation_result(InvalidationRes,
                                      invalidate_by_alternate_key,
                                      InvalidationDur);
do_invalidate_by_alternate_key(AltKey, #{store := _} = Opts) ->
    do_invalidate_by_alternate_key([AltKey], Opts).

%%====================================================================
%% Internal functions related to response selection
%%====================================================================

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

init_opts(#{store := Store} = Opts0) when is_atom(Store) ->
    Opts1 =
        case Opts0 of
            #{auto_compress := true} ->
                maps:put(auto_decompress, true, Opts0);
            _ ->
                Opts0
        end,
    maps:merge(?DEFAULT_OPTS, Opts1);
init_opts(_Opts) ->
    erlang:error(no_store_configured).

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
                           #{<<"content-encoding">> := ContentEncodings} = ParsedRespHeaders},
                     #{auto_decompress := true})
    when is_list(ContentEncodings) andalso not ?has_strong_etag(ParsedRespHeaders) ->
    not ?is_no_transform(ParsedRespHeaders)
    andalso lists:last(ContentEncodings) == <<"gzip">>;
varying_header_match(#{<<"accept-encoding">> := <<_/binary>> = AcceptEncoding},
                     <<"accept-encoding">>,
                     _RespHeaderValue,
                     #{parsed_headers :=
                           #{<<"content-encoding">> := ContentEncodings} = ParsedRespHeaders},
                     #{auto_accept_encoding := true} = Opts)
    when is_list(ContentEncodings) andalso not ?is_no_transform(ParsedRespHeaders) ->
    ContentEncoding = lists:last(ContentEncodings),
    AcceptedEncodings =
        [Encoding
         || {Encoding, Priority} <- cow_http_hd:parse_accept_encoding(AcceptEncoding),
            Priority > 0],
    lists:member(ContentEncoding, AcceptedEncodings)
    orelse not ?has_strong_etag(ParsedRespHeaders)
           andalso map_get(auto_decompress, Opts) == true;
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
                                                                     Opts),

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
                         #{backend_in_error := true},
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

%%====================================================================
%% Internal functions related to response post-processing
%%====================================================================

set_age_header({Status, RespHeaders0, RespBody}, #{created := Created}) ->
    AgeBin = list_to_binary(integer_to_list(unix_now() - Created)),
    RespHeaders1 = delete_header(<<"age">>, RespHeaders0),
    RespHeaders2 = set_header_value(<<"age">>, AgeBin, RespHeaders1),
    {Status, RespHeaders2, RespBody}.

handle_file_body({_Status, _RespHeaders, {sendfile, _, _, _}} = Response) ->
    Response;
handle_file_body({Status, RespHeaders, {file, FilePath}}) ->
    {Status, RespHeaders, {sendfile, 0, all, FilePath}};
handle_file_body(Response) ->
    Response.

%%====================================================================
%% Internal functions related to response caching
%%====================================================================

% Invalidating POST requests is a grey zone: POST requests can be cached when there is
% explicit cache information (RFC7231 section 4.3.3) but POST requests are unsafe and must also
% trigger invalidation of the response for the URI (RFC9111 4.4).
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

handle_invalidation_of_unsafe_method({_, Url, _, _},
                                     [],
                                     #{store := Store, store_opts := StoreOpts} = Opts) ->
    Store:invalidate_url(url_digest(Url, Opts), StoreOpts);
handle_invalidation_of_unsafe_method(Request,
                                     [{HeaderName, HeaderValue} | RestHeaders],
                                     #{store := Store, store_opts := StoreOpts} = Opts) ->
    case ?LOWER(HeaderName) of
        <<"location">> ->
            Store:invalidate_url(url_digest(HeaderValue, Opts), StoreOpts);
        <<"content-location">> ->
            Store:invalidate_url(url_digest(HeaderValue, Opts), StoreOpts);
        _ ->
            ok
    end,
    handle_invalidation_of_unsafe_method(Request, RestHeaders, Opts).

is_cacheable(Method, ParsedReqHeaders, Status, ParsedRespHeaders, Opts) ->
    request_method_cacheable(Method, ParsedRespHeaders, Opts)
    andalso response_status_is_final(Status)
    andalso response_status_cacheable(Status, ParsedRespHeaders)
    andalso not
                is_map_key(<<"no-store">>, maps:get(<<"cache-control">>, ParsedReqHeaders, #{}))
    andalso not
                is_map_key(<<"no-store">>, maps:get(<<"cache-control">>, ParsedRespHeaders, #{}))
    andalso (cache_type(Opts) == private
             orelse not
                        is_map_key(<<"private">>,
                                   maps:get(<<"cache-control">>, ParsedRespHeaders, #{})))
    andalso (cache_type(Opts) == private
             orelse not is_map_key(<<"authorization">>, ParsedReqHeaders)
             orelse response_explicitely_cacheable(ParsedRespHeaders))
    andalso (is_map_key(<<"public">>, maps:get(<<"cache-control">>, ParsedRespHeaders, #{}))
             orelse cache_type(Opts) == private
                    andalso is_map_key(<<"private">>,
                                       maps:get(<<"cache-control">>, ParsedRespHeaders, #{}))
             orelse is_map_key(<<"expires">>, ParsedRespHeaders)
             orelse is_map_key(<<"max-age">>, maps:get(<<"cache-control">>, ParsedRespHeaders, #{}))
             orelse cache_type(Opts) == shared
                    andalso is_map_key(<<"s-maxage">>,
                                       maps:get(<<"cache-control">>, ParsedRespHeaders, #{}))
             orelse response_status_heuristically_cacheable(Status))
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

response_status_is_final(Status) when Status >= 100, Status < 200 ->
    false;
response_status_is_final(_) ->
    true.

response_status_cacheable(206, _) ->
    false;
response_status_cacheable(304, _) ->
    false;
response_status_cacheable(Status,
                          #{<<"cache-control">> := #{<<"must-understand">> := _}}) ->
    response_status_heuristically_cacheable(Status);
response_status_cacheable(_, _) ->
    true.

response_status_heuristically_cacheable(200) ->
    true;
response_status_heuristically_cacheable(203) ->
    true;
response_status_heuristically_cacheable(204) ->
    true;
response_status_heuristically_cacheable(300) ->
    true;
response_status_heuristically_cacheable(301) ->
    true;
response_status_heuristically_cacheable(308) ->
    true;
response_status_heuristically_cacheable(404) ->
    true;
response_status_heuristically_cacheable(405) ->
    true;
response_status_heuristically_cacheable(410) ->
    true;
response_status_heuristically_cacheable(414) ->
    true;
response_status_heuristically_cacheable(451) ->
    true;
response_status_heuristically_cacheable(501) ->
    true;
response_status_heuristically_cacheable(_) ->
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
corrected_age_value(Age, _Opts) when is_integer(Age) ->
    Age;
corrected_age_value(_, _Opts) ->
    undefined.

expires(CreatedAt,
        _MaybeDate,
        #{<<"cache-control">> := #{<<"s-maxage">> := SMaxAge}},
        #{type := shared}) ->
    {header, CreatedAt + SMaxAge};
expires(CreatedAt,
        _MaybeDate,
        #{<<"cache-control">> := #{<<"max-age">> := MaxAge}},
        _Opts) ->
    {header, CreatedAt + MaxAge};
expires(CreatedAt, Date, #{<<"expires">> := Expires}, _Opts) when is_integer(Date) ->
    {header, CreatedAt + Expires - Date};
expires(_CreatedAt, _MaybeDate, #{<<"expires">> := Expires}, _Opts) ->
    {header, Expires};
expires(CreatedAt, _MaybeDate, _ParsedRespHeaders, #{default_ttl := DefaultTTL}) ->
    {heuristics, CreatedAt + DefaultTTL}.

grace(Expires, #{default_grace := Grace}) ->
    Expires + Grace.

cache_type(#{type := Type}) ->
    Type.

check_set_cookie(_, #{prevent_set_cookie := false}) ->
    ok;
check_set_cookie(_, #{prevent_set_cookie := auto, type := private}) ->
    ok;
check_set_cookie([], _Opts) ->
    ok;
check_set_cookie([{HeaderName, _} | Rest], Opts) ->
    case ?LOWER(HeaderName) of
        <<"set-cookie">> ->
            error(set_cookie_header_forbidden);
        _ ->
            check_set_cookie(Rest, Opts)
    end.

%%====================================================================
%% Internal functions related to compression
%%====================================================================

handle_auto_decompress(ParsedReqHeaders,
                       {Status, RespHeaders0, BodyOrFile} = Response,
                       #{parsed_headers :=
                             #{<<"content-encoding">> := ContentEncodings} = ParsedRespHeaders},
                       #{auto_decompress := true})
    when is_list(ContentEncodings) andalso not ?has_strong_etag(ParsedRespHeaders) ->
    ContentEncoding = lists:last(ContentEncodings),
    AcceptedEncodings =
        [AcceptedEncoding
         || {AcceptedEncoding, Priority}
                <- maps:to_list(
                       maps:get(<<"accept-encoding">>, ParsedReqHeaders, #{})),
            Priority > 0],
    case lists:member(ContentEncoding, AcceptedEncodings) of
        true ->
            Response;
        false ->
            CompressedBody = get_body_content(BodyOrFile),
            DecompressedBody = zlib:gunzip(CompressedBody),
            telemetry:execute(?TELEMETRY_DECOMPRESS_EVT, #{}, #{alg => gzip}),
            ContentLengthBin = list_to_binary(integer_to_list(byte_size(DecompressedBody))),
            RespHeaders1 = delete_header(<<"content-encoding">>, RespHeaders0),
            RespHeaders2 =
                case ContentEncodings of
                    [_] ->
                        RespHeaders1;
                    SeveralContentEncodings ->
                        AllEncodingsButLast =
                            lists:reverse(tl(lists:reverse(SeveralContentEncodings))),
                        ContentEncodingsBin =
                            iolist_to_binary(lists:join(<<", ">>, AllEncodingsButLast)),
                        set_header_value(<<"content-encoding">>, ContentEncodingsBin, RespHeaders1)
                end,
            RespHeaders3 = set_header_value(<<"content-length">>, ContentLengthBin, RespHeaders2),
            {Status, RespHeaders3, DecompressedBody}
    end;
handle_auto_decompress(_ParsedReqHeaders, Response, _RespMetadata, _Opts) ->
    Response.

%%====================================================================
%% Internal functions related to range requests
%%====================================================================

handle_range_request(_Request,
                     #{<<"cache-control">> := #{<<"no-transform">> := _}},
                     Response,
                     _RespMetadata,
                     _Opts) ->
    Response;
handle_range_request(_Request,
                     _ParsedReqHeaders,
                     Response,
                     #{parsed_headers := #{<<"cache-control">> := #{<<"no-transform">> := _}}},
                     _Opts) ->
    Response;
handle_range_request({<<"GET">>, _, _, _},
                     % cowlib parse empty bytes without raising
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
         andalso not (Date - LastModified >= 1 andalso LastModified =< IfUnmodifiedSince) ->
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
           build_range_response(NormByteRangeSpec, BodySize, Response, RespMetadata);
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
            throw(resp_body_no_longer_available)
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

build_range_response([], BodySize, _Response, _RespMetadata) ->
    range_not_satisfiable_resp(BodySize);
build_range_response([{StartOffset, Length}],
                     BodySize,
                     {_Status, RespHeaders0, BodyOrFile},
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
    case BodyOrFile of
        <<_/binary>> = BinBody ->
            {206, RespHeaders3, binary:part(BinBody, StartOffset, Length)};
        {file, FilePath} ->
            {206, RespHeaders3, {sendfile, StartOffset, Length, FilePath}}
    end;
build_range_response(NormByteRangeSpec,
                     BodySize,
                     {_Status, RespHeaders, _Body} = Response,
                     RespMetadata) ->
    [{{FirstStartOffset, FirstLength}, FirstChunk} | OtherChunks] =
        get_range_chunks(NormByteRangeSpec, Response),
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

get_range_chunks(NormByteRangeSpec, {_, _, {file, Filename}}) ->
    case file:open(Filename, [read, raw, binary]) of
        {ok, File} ->
            case file:pread(File, NormByteRangeSpec) of
                {ok, Chunks} ->
                    file:close(File),
                    lists:zip(NormByteRangeSpec, Chunks);
                {error, _} ->
                    file:close(File),
                    throw(resp_body_no_longer_available)
            end;
        {error, _} ->
            throw(resp_body_no_longer_available)
    end;
get_range_chunks(NormByteRangeSpec, {_, _, Body}) ->
    [{Range, binary:part(Body, StartOffset, Length)}
     || {StartOffset, Length} = Range <- NormByteRangeSpec].

content_range(StartOffset, Length, BodySize) ->
    BodySizeBin = list_to_binary(integer_to_list(BodySize)),
    StartOffsetBin = list_to_binary(integer_to_list(StartOffset)),
    EndOffsetBin = list_to_binary(integer_to_list(StartOffset + Length - 1)),
    <<"bytes ", StartOffsetBin/binary, "-", EndOffsetBin/binary, "/", BodySizeBin/binary>>.

range_not_satisfiable_resp(BodySize) ->
    BodySizeBin = list_to_binary(integer_to_list(BodySize)),
    {416, [{<<"content-range">>, <<"bytes */", BodySizeBin/binary>>}], <<"">>}.

%%====================================================================
%% Internal functions related to conditional requests
%%====================================================================

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

%%====================================================================
%% Internal functions related to response refresh
%%====================================================================

refresh_stored_responses(Request,
                         {304, RespHeaders, _},
                         #{store := Store, store_opts := StoreOpts} = Opts) ->
    RequestKey = request_key(Request, Opts),
    Candidates = Store:list_candidates(RequestKey, StoreOpts),
    ParsedRespHeaders = parse_headers(RespHeaders, [<<"etag">>, <<"last-modified">>]),
    CandidatesToUpdate = select_candidates_to_update(Candidates, ParsedRespHeaders),
    update_candidates(CandidatesToUpdate, Request, RespHeaders, update_headers, Opts);
refresh_stored_responses({<<"HEAD">>, Url, ReqHeaders, ReqBody},
                         {200, RespHeaders, _},
                         #{store := Store, store_opts := StoreOpts} = Opts) ->
    RequestKey = request_key({<<"GET">>, Url, ReqHeaders, ReqBody}, Opts),
    Candidates = Store:list_candidates(RequestKey, StoreOpts),
    ParsedRespHeaders =
        parse_headers(RespHeaders, [<<"etag">>, <<"last-modified">>, <<"content-length">>]),
    CandidatesToUpdate =
        lists:filter(fun(Candidate) -> is_updatable_by_head_resp(Candidate, ParsedRespHeaders)
                     end,
                     Candidates),
    CandidatesToInvalidate = Candidates -- CandidatesToUpdate,
    GetRequest = {<<"GET">>, Url, ReqHeaders, ReqBody},
    update_candidates(CandidatesToUpdate, GetRequest, RespHeaders, update_headers, Opts),
    update_candidates(CandidatesToInvalidate, GetRequest, RespHeaders, invalidate, Opts).

select_candidates_to_update(Candidates, #{<<"etag">> := {strong, _} = ETag}) ->
    lists:filter(fun ({_, _, _, _, #{parsed_headers := #{<<"etag">> := CandidateETag}}})
                         when CandidateETag == ETag ->
                         true;
                     (_) ->
                         false
                 end,
                 Candidates);
select_candidates_to_update(Candidates, #{<<"etag">> := {weak, _} = ETag}) ->
    case lists:filter(fun ({_, _, _, _, #{parsed_headers := #{<<"etag">> := CandidateETag}}})
                              when CandidateETag == ETag ->
                              true;
                          (_) ->
                              false
                      end,
                      Candidates)
    of
        [] ->
            [];
        SelectedCandidates ->
            [most_recent_candidate(SelectedCandidates)]
    end;
select_candidates_to_update(Candidates, #{<<"last-modified">> := LastModified}) ->
    case lists:filter(fun ({_,
                            _,
                            _,
                            _,
                            #{parsed_headers := #{<<"last-modified">> := CandidateLastModified}}})
                              when CandidateLastModified == LastModified ->
                              true;
                          (_) ->
                              false
                      end,
                      Candidates)
    of
        [] ->
            [];
        SelectedCandidates ->
            [most_recent_candidate(SelectedCandidates)]
    end;
select_candidates_to_update([{_, _, _, _, #{parsed_headers := ParsedHeaders}} =
                                 Candidate],
                            _)
    when not
             (is_map_key(<<"etag">>, ParsedHeaders)
              orelse is_map_key(<<"last-modified">>, ParsedHeaders)) ->
    [Candidate];
select_candidates_to_update(_, _) ->
    [].

most_recent_candidate(Candidates) ->
    element(2,
            lists:max([{CreatedAt, Candidate}
                       || {_, _, _, _, #{created := CreatedAt}} = Candidate <- Candidates])).

is_updatable_by_head_resp({_,
                           _,
                           _,
                           _,
                           #{parsed_headers :=
                                 #{<<"etag">> := Etag, <<"content-length">> := ContentLength}}},
                          #{<<"etag">> := Etag, <<"content-length">> := ContentLength}) ->
    true;
is_updatable_by_head_resp({_, _, _, _, #{parsed_headers := #{<<"etag">> := Etag}}},
                          #{<<"etag">> := Etag}) ->
    true;
is_updatable_by_head_resp({_,
                           _,
                           _,
                           _,
                           #{parsed_headers :=
                                 #{<<"last-modified">> := LastModified,
                                   <<"content-length">> := ContentLength}}},
                          #{<<"last-modified">> := LastModified,
                            <<"content-length">> := ContentLength}) ->
    true;
is_updatable_by_head_resp({_,
                           _,
                           _,
                           _,
                           #{parsed_headers := #{<<"last-modified">> := LastModified}}},
                          #{<<"last-modified">> := LastModified}) ->
    true;
is_updatable_by_head_resp(_, _) ->
    false.

update_candidates([], _Request, _RespHeaders, _Type, _Opts) ->
    ok;
update_candidates([{RespRef, _, _, _, #{alternate_keys := AltKeys}} | Rest],
                  Request,
                  RespHeaders,
                  Type,
                  #{store := Store, store_opts := StoreOpts} = Opts) ->
    try
        {Status, StoredRespHeaders, BodyOrFile, _} = Store:get_response(RespRef, StoreOpts),
        RespBody = get_body_content(BodyOrFile),
        UpdatedRespHeaders =
            case Type of
                update_headers ->
                    update_headers(StoredRespHeaders, RespHeaders);
                invalidate ->
                    Now = timestamp_to_rfc7231(unix_now()),
                    set_header_value(<<"expires">>, Now, RespHeaders)
            end,

        UpdatedOpts = maps:put(alternate_keys, AltKeys, Opts),

        analyze_cache(Request, {Status, UpdatedRespHeaders, RespBody}, UpdatedOpts)
    catch
        _:_ ->
            ok
    end,
    update_candidates(Rest, Request, RespHeaders, Type, Opts).

%%====================================================================
%% Internal util functions for headers
%%====================================================================

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

update_headers(OldHeaders, NewHeaders) ->
    HeadersToUpdate = sets:from_list([?LOWER(HeaderName) || {HeaderName, _} <- NewHeaders]),
    lists:filter(fun({HeaderName, _}) ->
                    not sets:is_element(?LOWER(HeaderName), HeadersToUpdate)
                 end,
                 OldHeaders)
    ++ NewHeaders.

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

strip_connection_headers([]) ->
    [];
strip_connection_headers([{Name, Value} | Rest]) ->
    case is_map_key(?LOWER(Name), ?RESP_HEADERS_TO_DELETE) of
        true ->
            strip_connection_headers(Rest);
        false ->
            [{Name, Value} | strip_connection_headers(Rest)]
    end.

%%====================================================================
%% Other internal util functions
%%====================================================================

get_body_content({file, FilePath}) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            Content;
        {error, _} ->
            throw(resp_body_no_longer_available)
    end;
get_body_content(<<_/binary>> = BinBody) ->
    BinBody.

telemetry_log_invalidation_result({ok, NbInvalidation}, Type, InvalidationDur)
    when is_integer(NbInvalidation) ->
    telemetry:execute(?TELEMETRY_INVALIDATION_EVT,
                      #{count => NbInvalidation, duration => InvalidationDur},
                      #{type => Type}),
    {ok, NbInvalidation};
telemetry_log_invalidation_result({ok, undefined}, Type, InvalidationDur) ->
    telemetry:execute(?TELEMETRY_INVALIDATION_EVT,
                      #{duration => InvalidationDur},
                      #{type => Type}),
    {ok, undefined};
telemetry_log_invalidation_result({error, Reason}, Type, _InvlidationDur) ->
    telemetry_log_error(Reason, Type).

telemetry_log_cache_store_result({error, Reason}) ->
    telemetry_log_error(Reason, cache);
telemetry_log_cache_store_result(_) ->
    ok.

telemetry_log_error(Reason, Type) ->
    telemetry:execute(?TELEMETRY_STORE_ERROR_EVT, #{}, #{type => Type, reason => Reason}).

unix_now() ->
    os:system_time(second).

now_monotonic_us() ->
    erlang:monotonic_time(microsecond).

timestamp_to_rfc7231(Timestamp) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Timestamp + 62167219200),
    cow_date:rfc7231(DateTime).

telemetry_start_measurement(Measurement) ->
    Measurements =
        case get({?TELEMETRY_PROCESS_TAG, measurements}) of
            undefined ->
                #{};
            Map ->
                Map
        end,
    put({?TELEMETRY_PROCESS_TAG, measurements},
        maps:put(Measurement, now_monotonic_us(), Measurements)).

telemetry_stop_measurement(Measurement) ->
    case get({?TELEMETRY_PROCESS_TAG, measurements}) of
        undefined ->
            ok;
        #{Measurement := StartValue} = Measurements ->
            put({?TELEMETRY_PROCESS_TAG, measurements},
                maps:put(Measurement, now_monotonic_us() - StartValue, Measurements))
    end.

telemetry_set_measurement(Measurement, Value) ->
    Measurements =
        case get({?TELEMETRY_PROCESS_TAG, measurements}) of
            undefined ->
                #{};
            Map ->
                Map
        end,
    put({?TELEMETRY_PROCESS_TAG, measurements}, maps:put(Measurement, Value, Measurements)).

telemetry_set_metadata(Key, Value) ->
    Metadata =
        case get({?TELEMETRY_PROCESS_TAG, metadata}) of
            undefined ->
                #{};
            Map ->
                Map
        end,
    put({?TELEMETRY_PROCESS_TAG, metadata}, maps:put(Key, Value, Metadata)).

telemetry_send_event(Event) ->
    case get({?TELEMETRY_PROCESS_TAG, measurements}) of
        undefined ->
            ok;
        Measurements ->
            Metadata =
                case get({?TELEMETRY_PROCESS_TAG, metadata}) of
                    undefined ->
                        #{};
                    Map ->
                        Map
                end,
            telemetry:execute(Event, Measurements, Metadata)
    end.
