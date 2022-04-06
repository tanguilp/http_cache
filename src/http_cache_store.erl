-module(http_cache_store).

-type url_digest() :: binary().
-type response_ref() :: term().
-type response_metadata() ::
    #{created := http_cache:timestamp(),
      expires := http_cache:timestamp(),
      grace := http_cache:timestamp(),
      ttl_set_by := header | heuristics,
      parsed_headers := #{binary() => term()},
      alternate_keys := [http_cache:alternate_key()],
      compressed_by_this_lib := boolean()}.
-type body() :: binary().

             % The body transmitted to the backend is a binary so as to optimize copying around
             % data: an IOlist would have to be copied whereas (big) binaries are simply
             % reference-counted.

%TODO: use {http_cache:response(), response_metadata()}
-type response() ::
    {Status :: http_cache:status(),
     Headers :: http_cache:headers(),
     BodyOrFile :: body() | {file, file:name_all()},
     Metadata :: response_metadata()}.
-type candidate() ::
    {RespRef :: response_ref(),
     Status :: http_cache:status(),
     RespHeaders :: http_cache:headers(),
     VaryHeaders :: http_cache:vary_headers(),
     RespMetadata :: response_metadata()}.

-callback list_candidates(RequestKey :: http_cache:request_key()) -> [candidate()].
-callback get_response(RespRef :: response_ref()) -> response() | undefined.
-callback put(RequestKey :: http_cache:request_key(),
              UrlDigest :: url_digest(),
              VaryHeaders :: http_cache:vary_headers(),
              Response :: http_cache:response(),
              RespMetadata :: response_metadata()) ->
                 ok | {error, term()}.
-callback invalidate_url(UrlDigest :: url_digest()) -> http_cache:invalidation_result().
-callback notify_resp_used(RespRef :: response_ref(), Time :: http_cache:timestamp()) ->
                              ok | {error, term()}.
-callback invalidate_by_alternate_key([AltKeys :: http_cache:alternate_key()]) ->
                                         http_cache:invalidation_result().

-optional_callbacks([invalidate_by_alternate_key/1]).
