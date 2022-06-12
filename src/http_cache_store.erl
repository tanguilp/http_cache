%%%-----------------------------------------------------------------------------
%%% @doc The behaviour for response stores
%%%
%%% Keep in mind that for a unique combination of a request's method, URL, body and bucket,
%%% there still can be several different responses, depending on the `vary' header. A so
%%% called candidate is a response that matches request information. The main `http_cache'
%%% module is in charge of selecting a response according to the vary header.
%%%
%%% This is why the process is the following:
%%% <ol>
%%%   <li> `http_cache' request all the potential responses (candidates) using `list_candidates/1' </li>
%%%   <li> `http_cache' selects the freshest response whose vary header matches </li>
%%%   <li> `http_cache' request the response with `get_response/1'</li>
%%% </ol>

-module(http_cache_store).

-export_type([request_key/0, candidate/0, response/0, response_ref/0, url_digest/0]).

-type url_digest() :: binary().
%% Opaque URL digest as computed by the main module
-type request_key() :: binary().
%% A unique, opaque, key for a request taking into account the request's information (method,
%% URL, body and bucket)
-type response_ref() :: http_cache_store:response_ref().
%% Opaque backend's reference to a response, returned by
%% {@link http_cache:get/2} and used as a parameter by {@link http_cache:notify_response_used/2}.
-type response_metadata() ::
    #{created := http_cache:timestamp(),
      expires := http_cache:timestamp(),
      grace := http_cache:timestamp(),
      ttl_set_by := header | heuristics,
      parsed_headers := #{binary() => term()},
      alternate_keys := [http_cache:alternate_key()]}.
-type body() :: binary().
% The body transmitted to the backend is a binary so as to optimize copying around
% data: an IOlist would have to be copied whereas (big) binaries are simply
% reference-counted.
-type vary_headers() :: #{binary() := binary() | undefined}.
%% Normalized headers on which the response varies
-type response() ::
    {Status :: http_cache:status(),
     Headers :: http_cache:headers(),
     BodyOrFile :: body() | {file, file:name_all()},
     Metadata :: response_metadata()}.
%% Stored HTTP response with its metadata. The body can either be a binary (for example if the
%% response is stored in memory) or a file (if the response is stored on disk).
-type candidate() ::
    {RespRef :: response_ref(),
     Status :: http_cache:status(),
     RespHeaders :: http_cache:headers(),
     VaryHeaders :: vary_headers(),
     RespMetadata :: response_metadata()}.
-type opts() :: any().

-callback list_candidates(RequestKey :: request_key(), Opts :: opts()) -> [candidate()].
%% Returns the list of candidates matching a request, via its request key
-callback get_response(RespRef :: response_ref(), Opts :: opts()) ->
                          response() | undefined.
%% Returns a response from a response reference returned by `list_candidates/1'
-callback put(RequestKey :: request_key(),
              UrlDigest :: url_digest(),
              VaryHeaders :: vary_headers(),
              Response :: http_cache:response(),
              RespMetadata :: response_metadata(),
              Opts :: opts()) ->
                 ok | {error, term()}.
%% Saves a response and associated metadata
-callback notify_response_used(RespRef :: response_ref(), Opts :: opts()) ->
                                  ok | {error, term()}.
%% Notify that a response was used. A LRU cache, for instance, would update the timestamp
%% the response was last used
-callback invalidate_url(UrlDigest :: url_digest(), Opts :: opts()) ->
                            http_cache:invalidation_result().
%% Invalidates all responses for a given URL digest
-callback invalidate_by_alternate_key([AltKeys :: http_cache:alternate_key()],
                                      Opts :: opts()) ->
                                         http_cache:invalidation_result().

%% Invalidates all responses that has been tag with one of the alternate keys

-optional_callbacks([invalidate_by_alternate_key/2]).
