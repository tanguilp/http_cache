%%%-----------------------------------------------------------------------------
%%% @doc An in-process implementation of {@link http_cache_store}
%%%
%%% This implementation is used in this library's test and can be used in your own tests.
%%% It stores data in the current process, and thus provides isolation. It cannot, of course, be
%%% used in real-life because data is discarded as soon as the process dies, and no cleanup
%%% ever happens.
%%%
%%% The {@link save_in_file/0} function can be used to simulate a store that saves responses
%%% on the disk. It saves the responses to files in `tmp' and therefore cannot be used on
%%% non-UNIX systems.

-module(http_cache_store_process).

-behaviour(http_cache_store_behaviour).

-export([list_candidates/2, get_response/2, put/6, invalidate_url/2,
         invalidate_by_alternate_key/2, notify_response_used/2, save_in_file/0]).

list_candidates(ReqKey, _Opts) ->
    Now = unix_now(),
    [{RespRef, Status, Headers, VaryHeaders, RespMetadata}
     || {{ObjReqKey, VaryHeaders} = RespRef,
         {{Status, Headers, _Body}, _UrlDigest, #{grace := Grace} = RespMetadata, _LastUsed}}
            <- get(),
        ObjReqKey == ReqKey,
        Grace >= Now].

get_response(RespRef, _Opts) ->
    case get(RespRef) of
        {{Status, Headers, Body}, _UrlDigest, RespMetadata, _LastUsed} ->
            {Status, Headers, Body, RespMetadata};
        undefined ->
            undefined
    end.

put(ReqKey,
    UrlDigest,
    VaryHeaders,
    {Status, RespHeaders, RespBody0},
    RespMetadata,
    _Opts) ->
    RespBody1 =
        case get({?MODULE, save_in_file}) of
            true ->
                FileName =
                    "/tmp/http_cache_test_"
                    ++ integer_to_list(erlang:unique_integer([positive]))
                    ++ ".txt",
                ok = file:write_file(FileName, RespBody0),
                {file, FileName};
            undefined ->
                RespBody0
        end,
    RespRef = {ReqKey, VaryHeaders},
    put(RespRef, {{Status, RespHeaders, RespBody1}, UrlDigest, RespMetadata, unix_now()}),
    ok.

invalidate_url(SearchedUrlDigest, _Opts) ->
    ToInvalidate =
        [RespRef
         || {RespRef, {_Response, UrlDigest, _RespMetadata, _LastUsed}} <- get(),
            SearchedUrlDigest == UrlDigest],
    NbInvalidated = length(ToInvalidate),
    [erase(RespRef) || RespRef <- ToInvalidate],
    {ok, NbInvalidated}.

invalidate_by_alternate_key(Searched, _Opts) ->
    ToInvalidate =
        [RespRef
         || {RespRef, {_Response, _UrlDigest, #{alternate_keys := AltKeys}, _LastUsed}} <- get(),
            lists:any(fun(Elt) -> lists:member(Elt, AltKeys) end, Searched)],
    NbInvalidated = length(ToInvalidate),
    [erase(RespRef) || RespRef <- ToInvalidate],
    {ok, NbInvalidated}.

notify_response_used(RespRef, _Opts) ->
    case get(RespRef) of
        {Response, UrlDigest, RespMetadata, _LastUsed} ->
            put(RespRef, {Response, UrlDigest, RespMetadata, unix_now()});
        undefined ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Saves response into a file
%%
%% When called before saving, it instructs this implementation to store the response in a file
%% in `/tmp'. Not thaat it works only on UNIX systems.
save_in_file() ->
    put({?MODULE, save_in_file}, true).

unix_now() ->
    os:system_time(second).
