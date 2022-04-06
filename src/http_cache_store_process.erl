-module(http_cache_store_process).

-behaviour(http_cache_store).

-export([list_candidates/1, get_response/1, put/5, invalidate_url/1,
         invalidate_by_alternate_key/1, notify_resp_used/2]).

list_candidates(ReqKey) ->
    Now = unix_now(),
    [{RespRef, Status, Headers, VaryHeaders, RespMetadata}
     || {{ObjReqKey, VaryHeaders} = RespRef,
         {{Status, Headers, _Body}, _UrlDigest, #{grace := Grace} = RespMetadata, _LastUsed}}
            <- get(),
        ObjReqKey == ReqKey,
        Grace >= Now].

get_response(RespRef) ->
    case get(RespRef) of
        {{Status, Headers, Body}, _UrlDigest, RespMetadata, _LastUsed} ->
            {Status, Headers, Body, RespMetadata};
        undefined ->
            undefined
    end.

put(ReqKey, UrlDigest, VaryHeaders, Response, RespMetadata) ->
    RespRef = {ReqKey, VaryHeaders},
    put(RespRef, {Response, UrlDigest, RespMetadata, unix_now()}),
    ok.

invalidate_url(SearchedUrlDigest) ->
    ToInvalidate =
        [RespRef
         || {RespRef, {_Response, UrlDigest, _RespMetadata, _LastUsed}} <- get(),
            SearchedUrlDigest == UrlDigest],
    NbInvalidated = length(ToInvalidate),
    [erase(RespRef) || RespRef <- ToInvalidate],
    {ok, NbInvalidated}.

invalidate_by_alternate_key(Searched) ->
    ToInvalidate =
        [RespRef
         || {RespRef, {_Response, _UrlDigest, #{alternate_keys := AltKeys}, _LastUsed}} <- get(),
            lists:any(fun(Elt) -> lists:member(Elt, AltKeys) end, Searched)],
    NbInvalidated = length(ToInvalidate),
    [erase(RespRef) || RespRef <- ToInvalidate],
    {ok, NbInvalidated}.

notify_resp_used(RespRef, Time) ->
    case get(RespRef) of
        {Response, UrlDigest, RespMetadata, _LastUsed} ->
            put(RespRef, {Response, UrlDigest, RespMetadata, Time});
        undefined ->
            ok
    end.

unix_now() ->
    os:system_time(second).
