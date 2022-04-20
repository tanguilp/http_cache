-module(http_cache_test).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_URL, <<"https://www.example.com">>).
-define(CACHEABLE_METHODS, [<<"HEAD">>, <<"GET">>]).
% source: http://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml
-define(ALL_STATUSES,
        [100, 101, 102, 103, 200, 201, 202, 203, 204, 205, 206, 207, 208, 226, 300, 301, 302, 303,
         305, 307, 308, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414,
         415, 416, 417, 421, 422, 423, 424, 425, 426, 428, 429, 431, 451, 500, 501, 502, 503, 504,
         505, 506, 507, 508, 510, 511]).
-define(DEFAULT_CACHEABLE_STATUSES,
        [200, 203, 204, 300, 301, 404, 405, 410, 414, 451, 501]).

http_cache_test_() ->
    {foreach,
     fun init/0,
     [fun opt_allow_stale_while_revalidate/1, fun opt_allow_stale_if_error_req_header/1,
      fun opt_allow_stale_if_error_resp_header/1, fun opt_auto_accept_encoding/1,
      fun opt_auto_compress/1, fun opt_auto_compress_strong_etags/1, fun opt_auto_decompress/1,
      fun opt_auto_decompress_multiple_content_encodings/1,
      fun opt_auto_decompress_when_body_is_a_file/1, fun opt_auto_decompress_strong_etags/1,
      fun opt_compression_threshold/1, fun opt_bucket/1, fun opt_origin_unreachable/1,
      fun opt_default_ttl/1, fun opt_ignore_query_params_order/1, fun opt_type/1,
      fun opt_request_time/1, fun invalidate_url/1, fun invalidate_by_alternate_key/1,
      fun invalidate_by_alternate_keys/1, fun response_stored_in_file_by_backend/1,
      fun cache_3_transforms_response_compression/1, fun cache_3_transforms_response_range/1,
      fun cache_4_transforms_response_compression/1, fun cache_4_transforms_response_range/1,
      fun rfc7234_section_3_method_cacheable/1, fun rfc7234_section_3_nostore_absent/1,
      fun rfc7234_section_3_private_absent/1, fun rfc7234_section_3_authz_header/1,
      fun rfc7234_section_3_resp_has_expires_ccdir/1,
      fun rfc7234_section_3_resp_has_maxage_ccdir/1,
      fun rfc7234_section_3_resp_has_smaxage_ccdir/1,
      fun rfc7234_section_3_resp_has_public_ccdir/1,
      fun rfc7234_section_3_1_range_response_not_cached/1,
      fun rfc7234_section_3_2_authorization_header_caching/1,
      fun rfc7234_section_4_req_nocache_ccdir/1, fun rfc7234_section_4_resp_nocache_ccdir/1,
      fun rfc7234_section_4_age_resp_header_generated/1,
      fun rfc7234_section_4_most_recent_resp/1, fun rfc7234_section_4_1_vary_header/1,
      fun rfc7234_section_4_2_stale_on_expired/1, fun rfc7234_section_4_2_1_smaxage_shared/1,
      fun rfc7234_section_4_2_1_smaxage_private/1, fun rfc7234_section_4_2_1_maxage/1,
      fun rfc7234_section_4_2_1_expires/1, fun rfc7234_section_4_2_2_heuristics_no_used/1,
      fun rfc7234_section_4_2_3_age_no_date_header/1,
      fun rfc7234_section_4_2_3_age_with_date_header/1,
      fun rfc7234_section_4_2_4_no_stale_returned_resp_ccdir_no_cache/1,
      fun rfc7234_section_4_2_4_no_stale_returned_resp_ccdir_no_store/1,
      fun rfc7234_section_4_2_4_no_stale_returned_resp_ccdir_must_revalidate/1,
      fun rfc7234_section_4_2_4_stale_returned_resp_ccdir_proxy_revalidate_priv_cache/1,
      fun rfc7234_section_4_2_4_no_stale_returned_resp_ccdir_proxy_revalidate_shared_cache/1,
      fun rfc7234_section_4_3_2_if_none_match_strong_etag/1,
      fun rfc7234_section_4_3_2_if_none_match_weak_etag/1,
      fun rfc7234_section_4_3_2_if_none_match_precondition_failed_for_side_effect_methods/1,
      fun rfc7234_section_4_3_2_if_modified_since/1,
      fun rfc7234_section_4_3_2_if_none_match_has_precedence_over_if_modified_since/1,
      fun rfc7234_section_4_3_2_if_range_with_etag/1,
      fun rfc7234_section_4_3_2_if_range_with_date/1,
      fun rfc7234_section_4_3_4_cached_response_updated_with_strong_validator_etag/1,
      fun rfc7234_section_4_3_4_cached_response_updated_with_strong_validator_etag_revalidation/1,
      fun rfc7234_section_4_3_4_cached_response_updated_with_weak_validator_last_modified/1,
      fun rfc7234_section_4_3_4_cached_response_updated_with_weak_validator_last_modified_revalidation/1,
      fun rfc7234_section_4_3_4_cached_response_updated_with_weak_validator_etag/1,
      fun rfc7234_section_4_3_4_cached_response_updated_with_weak_validator_etag_revalidation/1,
      fun rfc7234_section_4_3_4_cached_response_updated_with_no_validator/1,
      fun rfc7234_section_4_3_4_cached_response_updated_with_no_validator_revalidation/1,
      fun rfc7234_section_4_3_5_cached_response_updated_with_etag_matching/1,
      fun rfc7234_section_4_3_5_cached_response_updated_with_etag_matching_and_content_length/1,
      fun rfc7234_section_4_3_5_cached_response_updated_with_last_modified_matching/1,
      fun rfc7234_section_4_3_5_cached_response_updated_with_last_modified_matching_and_content_length/1,
      fun rfc7234_section_4_3_5_cached_response_invalidated_no_validator/1,
      fun rfc7234_section_4_3_5_cached_response_invalidated_no_validator_get_resp/1,
      fun rfc7234_section_4_3_5_cached_response_invalidated_no_validator_head_resp/1,
      fun rfc7234_section_4_3_5_cached_response_invalidated_content_length_mismatch/1,
      fun rfc7234_section_4_3_5_ignore_when_status_code_is_not_200/1,
      fun rfc7234_section_4_4_invalidate_uri_of_unsafe_method_on_non_error_status/1,
      fun rfc7234_section_4_4_no_invalidate_uri_of_unsafe_method_on_error_status/1,
      %TODO
      %fun rfc7234_section_5_2_cache_control_formatting/1,
      fun rfc7234_section_5_2_1_1_ccdir_max_age/1,
      fun rfc7234_section_5_2_1_2_ccdir_max_stale/1,
      fun rfc7234_section_5_2_1_3_ccdir_min_fresh/1,
      fun rfc7234_section_5_2_1_4_ccdir_no_cache/1,
      fun rfc7234_section_5_2_1_5_ccdir_no_store/1,
      fun rfc7234_section_5_2_1_7_ccdir_only_if_cached/1,
      fun rfc7234_section_5_2_2_1_ccdir_must_revalidate/1,
      fun rfc7234_section_5_2_2_2_ccdir_no_cache/1,
      fun rfc7234_section_5_2_2_3_ccdir_no_store/1,
      fun rfc7234_section_5_2_2_4_ccdir_no_transform_auto_compress/1,
      fun rfc7234_section_5_2_2_4_ccdir_no_transform_auto_decompress/1,
      fun rfc7234_section_5_2_2_4_ccdir_no_transform_range/1,
      fun rfc7234_section_5_2_2_5_ccdir_public/1, fun rfc7234_section_5_2_2_6_ccdir_private/1,
      fun rfc7234_section_5_2_2_7_ccdir_proxy_revalidate/1,
      fun rfc7234_section_5_2_2_8_ccdir_max_age/1, fun rfc7234_section_5_2_2_9_ccdir_s_maxage/1,
      fun rfc7234_section_5_3_header_expires_malformed/1,
      fun rfc7234_section_5_3_header_expires/1, fun rfc7234_section_5_4_header_pragma/1,
      fun rfc7234_section_5_5_1_response_is_stale/1,
      %fun rfc7234_section_5_5_2_warning_revalidation_failed/1,
      fun rfc7234_section_5_5_4_warning_heuristics_expiration/1,
      fun rfc7234_section_5_5_6_plain_compressed/1,
      fun rfc7234_section_5_5_6_already_compressed/1,
      fun rfc5861_stale_while_revalidate_not_expired/1,
      fun rfc5861_stale_while_revalidate_expired/1,
      fun rfc5861_stale_if_error_req_not_expired/1, fun rfc5861_stale_if_error_req_expired/1,
      fun rfc5861_stale_if_error_resp_not_expired/1, fun rfc5861_stale_if_error_resp_expired/1,
      fun rfc7233_single_byte_range/1, fun rfc7233_single_byte_range_headers/1,
      fun rfc7233_multiple_byte_range/1, fun rfc7233_unknown_range_type/1,
      fun rfc7233_no_satisfiable_range/1,
      fun rfc7233_no_satisfiable_range_content_range_header/1,
      fun rfc7233_range_ignored_not_get/1, fun rfc7233_error_on_too_many_ranges/1]}.

opt_allow_stale_while_revalidate(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0, stale-while-revalidate=60">>}],
                             <<"Some content">>},
                            Opts)
        end,
    [{spawn,
      ?_assertMatch({stale, _},
                    begin
                        Store(),
                        http_cache:get(Req, [{allow_stale_while_revalidate, true} | Opts])
                    end)},
     {spawn,
      ?_assertMatch({must_revalidate, _},
                    begin
                        Store(),
                        http_cache:get(Req, Opts)
                    end)}].

opt_allow_stale_if_error_req_header(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [{<<"cache-control">>, <<"stale-if-error=60">>}], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=0">>}], <<"Some content">>},
                            Opts)
        end,
    [{spawn,
      ?_assertMatch({stale, _},
                    begin
                        Store(),
                        http_cache:get(Req, [{allow_stale_if_error, true} | Opts])
                    end)},
     {spawn,
      ?_assertMatch({must_revalidate, _},
                    begin
                        Store(),
                        http_cache:get(Req, Opts)
                    end)}].

opt_allow_stale_if_error_resp_header(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0, stale-if-error=60">>}],
                             <<"Some content">>},
                            Opts)
        end,
    [{spawn,
      ?_assertMatch({stale, _},
                    begin
                        Store(),
                        http_cache:get(Req, [{allow_stale_if_error, true} | Opts])
                    end)},
     {spawn,
      ?_assertMatch({must_revalidate, _},
                    begin
                        Store(),
                        http_cache:get(Req, Opts)
                    end)}].

opt_auto_accept_encoding(Opts) ->
    F = fun(Headers) ->
           http_cache:cache({<<"GET">>,
                             ?TEST_URL,
                             [{<<"accept-encoding">>, <<"br, compress">>},
                              {<<"accept-encoding">>, <<"gzip,deflate">>}],
                             <<"">>},
                            {200,
                             [{<<"content-encoding">>, <<"gzip">>},
                              {<<"vary">>, <<"accept-encoding">>}],
                             <<"Some encoded content">>},
                            Opts),
           http_cache:get({<<"GET">>, ?TEST_URL, Headers, <<"">>},
                          set_opt(auto_accept_encoding, true, Opts))
        end,
    [{spawn, ?_assertMatch({fresh, _}, begin F([{<<"accept-encoding">>, <<"gzip">>}]) end)},
     {spawn, ?_assertMatch({fresh, _}, begin F([{<<"accept-encoding">>, <<"gzip,br">>}]) end)},
     {spawn,
      ?_assertMatch({fresh, _}, begin F([{<<"accept-encoding">>, <<"gzip, br">>}]) end)},
     {spawn,
      ?_assertMatch({fresh, _}, begin F([{<<"accept-encoding">>, <<"br, gzip">>}]) end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        F([{<<"accept-encoding">>, <<"br, compress">>},
                           {<<"accept-encoding">>, <<"gzip,deflate">>}])
                    end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        F([{<<"accept-encoding">>,
                            <<"br;q=1.0, compress;q=1.0, gzip;q=1.0,deflate;q=1.0">>}])
                    end)},
     {spawn, ?_assertNotMatch({fresh, _}, begin F([]) end)},
     {spawn, ?_assertNotMatch({fresh, _}, begin F([{<<"accept-encoding">>, <<"br">>}]) end)},
     {spawn,
      ?_assertNotMatch({fresh, _}, begin F([{<<"accept-encoding">>, <<"gzipv2">>}]) end)},
     {spawn,
      ?_assertNotMatch({fresh, _},
                       begin
                           F([{<<"accept-encoding">>,
                               <<"br;q=1.0, compress;q=1.0, gzip;q=0.0,deflate;q=1.0">>}])
                       end)}].

opt_auto_compress(Opts) ->
    Body = <<"Some content">>,
    GzippedBody = zlib:gzip(Body),
    F = fun(ReqHeaders) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"content-type">>, <<"text/plain; charset=utf-8">>},
                              {<<"etag">>, <<"W/\"some-weak-etag\"">>}],
                             Body},
                            set_opt(auto_compress, true, Opts)),
           {fresh, {_, {200, RespHeaders, RespBody}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, ReqHeaders, <<"">>},
                              set_opt(auto_compress, true, Opts)),
           {proplists:get_value(<<"content-encoding">>, RespHeaders),
            proplists:get_value(<<"vary">>, RespHeaders),
            RespBody}
        end,
    [{spawn, ?_assertMatch({undefined, <<"accept-encoding">>, Body}, F([]))},
     {spawn,
      ?_assertMatch({<<"gzip">>, <<"accept-encoding">>, GzippedBody},
                    F([{<<"accept-encoding">>, <<"gzip">>}]))}].

opt_auto_compress_strong_etags(Opts) ->
    Body = <<"Some content">>,
    F = fun(ReqHeaders) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"content-type">>, <<"text/plain; charset=utf-8">>},
                              {<<"etag">>, <<"\"some-strong-etag\"">>}],
                             Body},
                            set_opt(auto_compress, true, Opts)),
           http_cache:get({<<"GET">>, ?TEST_URL, ReqHeaders, <<"">>},
                          set_opt(auto_compress, true, Opts))
        end,
    [{spawn, ?_assertMatch({fresh, {_, {_, _, Body}}}, F([]))},
     {spawn,
      ?_assertMatch({fresh, {_, {_, _, Body}}}, F([{<<"accept-encoding">>, <<"gzip">>}]))}].

opt_auto_decompress(Opts) ->
    Body = <<"Some content">>,
    GzippedBody = zlib:gzip(Body),
    F = fun(ReqHeaders) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [{<<"accept-encoding">>, <<"gzip">>}], <<"">>},
                            {200,
                             [{<<"content-encoding">>, <<"gzip">>},
                              {<<"vary">>, <<"accept-encoding">>}],
                             GzippedBody},
                            set_opt(auto_decompress, true, Opts)),
           {fresh, {_, {200, RespHeaders, RespBody}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, ReqHeaders, <<"">>},
                              set_opt(auto_accept_encoding,
                                      true,
                                      set_opt(auto_decompress, true, Opts))),
           {proplists:get_value(<<"content-encoding">>, RespHeaders),
            proplists:get_value(<<"vary">>, RespHeaders),
            RespBody}
        end,
    [{spawn, ?_assertMatch({undefined, <<"accept-encoding">>, Body}, F([]))},
     {spawn,
      ?_assertMatch({undefined, <<"accept-encoding">>, Body},
                    F([{<<"accept-encoding">>, <<"br, identity">>}]))},
     {spawn,
      ?_assertMatch({<<"gzip">>, <<"accept-encoding">>, GzippedBody},
                    F([{<<"accept-encoding">>, <<"gzip">>}]))}].

opt_auto_decompress_multiple_content_encodings(Opts) ->
    Body = <<"Some content">>,
    GzippedBody = zlib:gzip(Body),
    F = fun() ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"content-encoding">>, <<"identity, gzip">>},
                              {<<"vary">>, <<"accept-encoding">>}],
                             GzippedBody},
                            set_opt(auto_decompress, true, Opts)),
           {fresh, {_, {200, RespHeaders, RespBody}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, [], <<"">>},
                              set_opt(auto_accept_encoding,
                                      true,
                                      set_opt(auto_decompress, true, Opts))),
           {proplists:get_value(<<"content-encoding">>, RespHeaders), RespBody}
        end,
    {spawn, ?_assertMatch({<<"identity">>, Body}, F())}.

opt_auto_decompress_when_body_is_a_file(Opts) ->
    Body = <<"Some content">>,
    GzippedBody = zlib:gzip(Body),
    F = fun() ->
           http_cache_store_process:save_in_file(),
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"content-encoding">>, <<"gzip">>},
                              {<<"vary">>, <<"accept-encoding">>}],
                             GzippedBody},
                            set_opt(auto_decompress, true, Opts)),
           http_cache:get({<<"GET">>, ?TEST_URL, [], <<"">>},
                          set_opt(auto_accept_encoding, true, set_opt(auto_decompress, true, Opts)))
        end,
    {spawn, ?_assertMatch({fresh, {_, {_, _, <<"Some content">>}}}, F())}.

opt_auto_decompress_strong_etags(Opts) ->
    Body = <<"Some content">>,
    GzippedBody = zlib:gzip(Body),
    F = fun(ReqHeaders) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [{<<"accept-encoding">>, <<"gzip">>}], <<"">>},
                            {200,
                             [{<<"content-encoding">>, <<"gzip">>},
                              {<<"vary">>, <<"accept-encoding">>},
                              {<<"etag">>, <<"\"some-strong-etag\"">>}],
                             GzippedBody},
                            set_opt(auto_decompress, true, Opts)),
           http_cache:get({<<"GET">>, ?TEST_URL, ReqHeaders, <<"">>},
                          set_opt(auto_accept_encoding, true, set_opt(auto_decompress, true, Opts)))
        end,
    [{spawn, ?_assertMatch(miss, F([]))},
     {spawn, ?_assertMatch(miss, F([{<<"accept-encoding">>, <<"identity">>}]))},
     {spawn, ?_assertMatch(miss, F([{<<"accept-encoding">>, <<"br, identity">>}]))},
     {spawn, ?_assertMatch({fresh, _}, F([{<<"accept-encoding">>, <<"identity, gzip">>}]))},
     {spawn, ?_assertMatch({fresh, _}, F([{<<"accept-encoding">>, <<"gzip">>}]))}].

opt_compression_threshold(Opts) ->
    Body = <<"Some content">>,
    GzippedBody = zlib:gzip(Body),
    F = fun(Threshold) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [{<<"content-type">>, <<"text/plain">>}], Body},
                            set_opt(compression_threshold,
                                    Threshold,
                                    set_opt(auto_compress, true, Opts)))
        end,
    [{spawn, ?_assertMatch({ok, {_, _, GzippedBody}}, F(0))},
     {spawn, ?_assertMatch({ok, {_, _, Body}}, F(1000))}].

opt_bucket(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    OptsWithBucket = [{bucket, another_bucket} | Opts],
    Store = fun() -> http_cache:cache(Req, {200, [], <<"Some content">>}, OptsWithBucket) end,
    [{spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(),
                        http_cache:get(Req, OptsWithBucket)
                    end)},
     {spawn,
      ?_assertMatch(miss,
                    begin
                        Store(),
                        http_cache:get(Req, Opts)
                    end)}].

opt_origin_unreachable(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=0">>}], <<"Some content">>},
                            Opts)
        end,
    [{spawn,
      ?_assertMatch({stale, _},
                    begin
                        Store(),
                        http_cache:get(Req, [{origin_unreachable, true} | Opts])
                    end)},
     {spawn,
      ?_assertMatch({must_revalidate, _},
                    begin
                        Store(),
                        http_cache:get(Req, Opts)
                    end)}].

opt_default_ttl(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun(TTL) ->
           http_cache:cache(Req, {200, [], <<"Some content">>}, [{default_ttl, TTL} | Opts])
        end,
    [{spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(60),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertMatch({must_revalidate, _},
                    begin
                        Store(0),
                        http_cache:get(Req, Opts)
                    end)}].

opt_ignore_query_params_order(Opts) ->
    Req = {<<"GET">>, <<"http://example.com/?a=1&b">>, [], <<"">>},
    ReqRev = {<<"GET">>, <<"http://example.com/?b&a=1">>, [], <<"">>},
    OptsIgnore = [{ignore_query_params_order, true} | Opts],
    Store =
        fun(DoIgnore) ->
           http_cache:cache(Req,
                            {200, [], <<"Some content">>},
                            [{ignore_query_params_order, DoIgnore} | Opts])
        end,
    [{spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(false),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertMatch(miss,
                    begin
                        Store(false),
                        http_cache:get(ReqRev, Opts)
                    end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(true),
                        http_cache:get(Req, OptsIgnore)
                    end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(true),
                        http_cache:get(ReqRev, OptsIgnore)
                    end)}].

opt_type(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    OptsPriv = set_opt(type, private, Opts),
    Store =
        fun(SelectedOpts) ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"private">>}], <<"Some content">>},
                            SelectedOpts)
        end,
    [{spawn,
      ?_assertMatch(miss,
                    begin
                        Store(Opts),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(OptsPriv),
                        http_cache:get(Req, OptsPriv)
                    end)}].

opt_request_time(Opts) ->
    Now = unix_now(),
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200, [{<<"Age">>, <<"2">>}], <<"Some content">>},
                            set_opt(request_time, Now - 3, Opts)),
           {fresh, {_, {_, Headers, _}}} = http_cache:get(Req, Opts),
           proplists:get_value(<<"age">>, Headers)
        end,
    {spawn, ?_assertEqual(<<"5">>, F())}.

invalidate_url(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req, {200, [], <<"Some content">>}, Opts),
           http_cache:invalidate_url(?TEST_URL, Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn, ?_assertEqual(miss, F())}.

invalidate_by_alternate_key(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    AltKey = {any, [#{term => works}]},
    F = fun() ->
           http_cache:cache(Req,
                            {200, [], <<"Some content">>},
                            set_opt(alternate_keys, [AltKey], Opts)),
           http_cache:invalidate_by_alternate_key(AltKey, Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn, ?_assertEqual(miss, F())}.

invalidate_by_alternate_keys(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    AltKey = {any, [#{term => works}]},
    F = fun() ->
           http_cache:cache(Req,
                            {200, [], <<"Some content">>},
                            set_opt(alternate_keys, [AltKey], Opts)),
           http_cache:invalidate_by_alternate_key([<<"some key">>, AltKey, <<"some other key">>],
                                                  Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn, ?_assertEqual(miss, F())}.

response_stored_in_file_by_backend(Opts) ->
    F = fun(ReqHeaders) ->
           http_cache_store_process:save_in_file(),
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [], <<"Some content">>},
                            Opts),
           http_cache:get({<<"GET">>, ?TEST_URL, ReqHeaders, <<"">>}, Opts)
        end,
    [{spawn, ?_assertMatch({fresh, {_, {_, _, {sendfile, 0, all, _}}}}, F([]))},
     {spawn,
      ?_assertMatch({fresh, {_, {_, _, {sendfile, 0, 4, _}}}},
                    F([{<<"range">>, <<"bytes=0-3">>}]))},
     {spawn,
      ?_assertMatch({fresh, {_, {206, _, _}}}, F([{<<"range">>, <<"bytes=0-3, 5-">>}]))}].

cache_3_transforms_response_compression(Opts) ->
    Body = <<"Some content">>,
    GzippedBody = zlib:gzip(Body),
    {spawn,
     ?_assertMatch({ok, {_, _, GzippedBody}},
                   http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                                    {200, [{<<"content-type">>, <<"text/plain">>}], Body},
                                    set_opt(auto_compress, true, Opts)))}.

cache_3_transforms_response_range(Opts) ->
    {spawn,
     ?_assertMatch({ok, {206, _, <<"Some">>}},
                   http_cache:cache({<<"GET">>,
                                     ?TEST_URL,
                                     [{<<"range">>, <<"bytes=0-3">>}],
                                     <<"">>},
                                    {200, [], <<"Some content">>},
                                    Opts))}.

cache_4_transforms_response_compression(Opts) ->
    Body = <<"Some content">>,
    GzippedBody = zlib:gzip(Body),
    {spawn,
     ?_assertMatch({ok, {200, _, GzippedBody}},
                   begin
                       {ok, RespToRevalidate} =
                           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                                            {200, [{<<"content-type">>, <<"text/plain">>}], Body},
                                            set_opt(auto_compress, true, Opts)),
                       http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                                        {304, [{<<"content-type">>, <<"text/plain">>}], <<>>},
                                        RespToRevalidate,
                                        set_opt(auto_compress, true, Opts))
                   end)}.

cache_4_transforms_response_range(Opts) ->
    {spawn,
     ?_assertMatch({ok, {206, _, <<"Some">>}},
                   begin
                       {ok, RespToRevalidate} =
                           http_cache:cache({<<"GET">>,
                                             ?TEST_URL,
                                             [{<<"range">>, <<"bytes=0-3">>}],
                                             <<"">>},
                                            {200, [], <<"Some content">>},
                                            Opts),
                       http_cache:cache({<<"GET">>,
                                         ?TEST_URL,
                                         [{<<"range">>, <<"bytes=0-3">>}],
                                         <<"">>},
                                        {304, [], <<>>},
                                        RespToRevalidate,
                                        Opts)
                   end)}.

rfc7234_section_3_method_cacheable(Opts) ->
    [?_assertMatch({ok, _},
                   http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                    {Status, [], <<"Some content">>},
                                    Opts))
     || Method <- [<<"HEAD">>, <<"GET">>], Status <- ?DEFAULT_CACHEABLE_STATUSES]
    ++ [?_assertMatch({ok, _},
                      http_cache:cache({<<"POST">>, ?TEST_URL, [], <<"">>},
                                       {Status, [CacheHeader], <<"Some content">>},
                                       Opts))
        || CacheHeader
               <- [{<<"cache-control">>, <<"s-maxage=3600">>},
                   {<<"cache-control">>, <<"max-age=3600">>},
                   {<<"expires">>, timestamp_to_rfc7231(unix_now() + 3600)}],
           Status <- ?DEFAULT_CACHEABLE_STATUSES].

rfc7234_section_3_nostore_absent(Opts) ->
    [[?_assertMatch(not_cacheable,
                    http_cache:cache({Method,
                                      ?TEST_URL,
                                      [{<<"cache-control">>, <<"no-store">>}],
                                      <<"">>},
                                     {Status, [], <<"Some content">>},
                                     Opts))
      || Method <- ?CACHEABLE_METHODS, Status <- ?DEFAULT_CACHEABLE_STATUSES],
     [?_assertMatch(not_cacheable,
                    http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                     {Status,
                                      [{<<"cache-control">>, <<"no-store">>}],
                                      <<"Some content">>},
                                     Opts))
      || Method <- ?CACHEABLE_METHODS, Status <- ?DEFAULT_CACHEABLE_STATUSES]].

rfc7234_section_3_private_absent(Opts) ->
    [[?_assertMatch(not_cacheable,
                    http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                     {Status,
                                      [{<<"cache-control">>, <<"private">>}],
                                      <<"Some content">>},
                                     Opts))
      || Method <- ?CACHEABLE_METHODS, Status <- ?DEFAULT_CACHEABLE_STATUSES],
     [?_assertMatch({ok, _},
                    http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                     {Status,
                                      [{<<"cache-control">>, <<"private">>}],
                                      <<"Some content">>},
                                     set_opt(type, private, Opts)))
      || Method <- ?CACHEABLE_METHODS, Status <- ?DEFAULT_CACHEABLE_STATUSES]].

rfc7234_section_3_authz_header(Opts) ->
    [[?_assertMatch(not_cacheable,
                    http_cache:cache({Method,
                                      ?TEST_URL,
                                      [{<<"authorization">>, <<"some-token">>}],
                                      <<"">>},
                                     {Status, [], <<"Some content">>},
                                     Opts))
      || Method <- ?CACHEABLE_METHODS, Status <- ?DEFAULT_CACHEABLE_STATUSES],
     [?_assertMatch({ok, _},
                    http_cache:cache({Method,
                                      ?TEST_URL,
                                      [{<<"authorization">>, <<"some-token">>}],
                                      <<"">>},
                                     {Status, [], <<"Some content">>},
                                     set_opt(type, private, Opts)))
      || Method <- ?CACHEABLE_METHODS, Status <- ?DEFAULT_CACHEABLE_STATUSES],
     [?_assertMatch({ok, _},
                    http_cache:cache({Method,
                                      ?TEST_URL,
                                      [{<<"authorization">>, <<"some-token">>}],
                                      <<"">>},
                                     {Status,
                                      [{<<"cache-control">>, CacheControlOpt}],
                                      <<"Some content">>},
                                     Opts))
      || Method <- ?CACHEABLE_METHODS,
         Status <- ?DEFAULT_CACHEABLE_STATUSES,
         CacheControlOpt <- [<<"must-revalidate">>, <<"public">>, <<"s-maxage=3600">>]]].

rfc7234_section_3_resp_has_expires_ccdir(Opts) ->
    [?_assertMatch({ok, _},
                   http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                    {Status,
                                     [{<<"expires">>, timestamp_to_rfc7231(unix_now() + 3600)}],
                                     <<"Some content">>},
                                    Opts))
     || Method <- ?CACHEABLE_METHODS, Status <- ?ALL_STATUSES -- ?DEFAULT_CACHEABLE_STATUSES].

rfc7234_section_3_resp_has_maxage_ccdir(Opts) ->
    [?_assertMatch({ok, _},
                   http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                    {Status,
                                     [{<<"cache-control">>, <<"max-age=3600">>}],
                                     <<"Some content">>},
                                    Opts))
     || Method <- ?CACHEABLE_METHODS, Status <- ?ALL_STATUSES -- ?DEFAULT_CACHEABLE_STATUSES].

rfc7234_section_3_resp_has_smaxage_ccdir(Opts) ->
    [[?_assertMatch({ok, _},
                    http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                     {Status,
                                      [{<<"cache-control">>, <<"s-maxage=3600">>}],
                                      <<"Some content">>},
                                     Opts))
      || Method <- ?CACHEABLE_METHODS, Status <- ?ALL_STATUSES -- ?DEFAULT_CACHEABLE_STATUSES],
     [?_assertMatch(not_cacheable,
                    http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                     {Status,
                                      [{<<"cache-control">>, <<"s-maxage=3600">>}],
                                      <<"Some content">>},
                                     set_opt(type, private, Opts)))
      || Method <- ?CACHEABLE_METHODS, Status <- ?ALL_STATUSES -- ?DEFAULT_CACHEABLE_STATUSES]].

rfc7234_section_3_resp_has_public_ccdir(Opts) ->
    [?_assertMatch({ok, _},
                   http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                    {Status,
                                     [{<<"cache-control">>, <<"public">>}],
                                     <<"Some content">>},
                                    Opts))
     || Method <- ?CACHEABLE_METHODS, Status <- ?ALL_STATUSES -- ?DEFAULT_CACHEABLE_STATUSES].

rfc7234_section_3_1_range_response_not_cached(Opts) ->
    [?_assertMatch(not_cacheable,
                   http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                                    {206,
                                     [{<<"content-range">>, <<"bytes 2-10/19">>}],
                                     <<"e cached">>},
                                    Opts))
     || Method <- ?CACHEABLE_METHODS].

rfc7234_section_3_2_authorization_header_caching(Opts) ->
    [[?_assertMatch(not_cacheable,
                    http_cache:cache({Method,
                                      ?TEST_URL,
                                      [{<<"authorization">>, <<"some-token">>}],
                                      <<"">>},
                                     {Status, [], <<"Some content">>},
                                     Opts))
      || Method <- ?CACHEABLE_METHODS, Status <- ?ALL_STATUSES],
     [?_assertMatch({ok, _},
                    http_cache:cache({Method,
                                      ?TEST_URL,
                                      [{<<"authorization">>, <<"some-token">>}],
                                      <<"">>},
                                     {Status,
                                      [{<<"cache-control">>, CacheControlOpt}],
                                      <<"Some content">>},
                                     Opts))
      || Method <- ?CACHEABLE_METHODS,
         Status <- ?DEFAULT_CACHEABLE_STATUSES,
         CacheControlOpt <- [<<"must-revalidate">>, <<"public">>, <<"s-maxage=3600">>]]].

%TODO: should we support this?
%rfc7234_section_4_head_of_get(Opts) ->
%  F = fun() -> http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>}, {200, [], <<"Some content">>}, Opts) end,
%  {
%    spawn,
%    ?_assertMatch({ok, _}, begin F(), http_cache:get({<<"HEAD">>, ?TEST_URL, [], <<"">>}, Opts) end)
%  }.

rfc7234_section_4_req_nocache_ccdir(Opts) ->
    [begin
         F = fun() ->
                http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                                 {200, [], <<"Some content">>},
                                 Opts)
             end,
         {spawn,
          ?_assertMatch({must_revalidate, _},
                        begin
                            F(),
                            http_cache:get({<<"GET">>,
                                            ?TEST_URL,
                                            [{CCHeader, <<"no-cache">>}],
                                            <<"">>},
                                           Opts)
                        end)}
     end
     || CCHeader <- [<<"pragma">>, <<"cache-control">>]].

rfc7234_section_4_resp_nocache_ccdir(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Resp = {200, [{<<"cache-control">>, <<"no-cache">>}], <<"Some content">>},
    Store = fun() -> http_cache:cache(Req, Resp, Opts) end,
    {spawn,
     ?_assertMatch({must_revalidate, _},
                   begin
                       Store(),
                       http_cache:get(Req, Opts)
                   end)}.

rfc7234_section_4_age_resp_header_generated(Opts) ->
    F = fun() ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [{<<"age">>, <<"42">>}], <<"Some content">>},
                            Opts),
           {fresh, {_RespRef, {_Status, RespHeaders, _Body}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, [], <<"">>}, Opts),
           proplists:get_all_values(<<"age">>, RespHeaders)
        end,
    {spawn, ?_assertEqual([<<"42">>], F())}.

rfc7234_section_4_most_recent_resp(Opts) ->
    % Actually they are the same request (same request key) so the second will erase the
    % first. But we keep the test just in case (and to make it future-proof)
    F = fun() ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"Date">>, timestamp_to_rfc7231(unix_now() - 2)}],
                             <<"Some content">>},
                            Opts),
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"Date">>, timestamp_to_rfc7231(unix_now() - 1)}],
                             <<"Some content">>},
                            Opts),
           {fresh, {_RespRef, {_Status, RespHeaders, _Body}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, [], <<"">>}, Opts),
           proplists:get_all_values(<<"age">>, RespHeaders)
        end,
    {spawn, ?_assertEqual([<<"1">>], F())}.

rfc7234_section_4_1_vary_header(Opts) ->
    Vary =
        fun() ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [{<<"tEst">>, <<"val">>}], <<"">>},
                            {200, [{<<"vary">>, <<"teST">>}], <<"Some content">>},
                            Opts)
        end,
    VaryMissing =
        fun() ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [{<<"vary">>, <<"Test">>}], <<"Some content">>},
                            Opts)
        end,
    VaryMany =
        fun() ->
           http_cache:cache({<<"GET">>,
                             ?TEST_URL,
                             [{<<"Test1">>, <<"val1">>},
                              {<<"tEst2">>, <<"val2">>},
                              {<<"teSt3">>, <<"val3">>},
                              {<<"tesT4">>, <<"val4">>}],
                             <<"">>},
                            {200, [{<<"vary">>, <<"Test1, Test2, Test3">>}], <<"Some content">>},
                            Opts)
        end,
    VaryNorm =
        fun() ->
           http_cache:cache({<<"GET">>,
                             ?TEST_URL,
                             [{<<"Test">>, <<"   val1    ">>}, {<<"teSt">>, <<"val2">>}],
                             <<"">>},
                            {200, [{<<"vary">>, <<"test">>}], <<"Some content">>},
                            Opts)
        end,
    Tests =
        [?_assertMatch({fresh, _},
                       begin
                           Vary(),
                           http_cache:get({<<"GET">>, ?TEST_URL, [{<<"Test">>, <<"val">>}], <<"">>},
                                          Opts)
                       end),
         ?_assertMatch(miss,
                       begin
                           Vary(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"tEst">>, <<"anotherval">>}],
                                           <<"">>},
                                          Opts)
                       end),
         ?_assertMatch(miss,
                       begin
                           Vary(),
                           http_cache:get({<<"GET">>, ?TEST_URL, [], <<"">>}, Opts)
                       end),
         ?_assertMatch(miss,
                       begin
                           VaryMissing(),
                           http_cache:get({<<"GET">>, ?TEST_URL, [{<<"teSt">>, <<"val">>}], <<"">>},
                                          Opts)
                       end),
         ?_assertMatch(miss,
                       begin
                           VaryMissing(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"tesT">>, <<"anotherval">>}],
                                           <<"">>},
                                          Opts)
                       end),
         ?_assertMatch({fresh, _},
                       begin
                           VaryMissing(),
                           http_cache:get({<<"GET">>, ?TEST_URL, [], <<"">>}, Opts)
                       end),
         ?_assertMatch({fresh, _},
                       begin
                           VaryMany(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"TEST1">>, <<"val1">>},
                                            {<<"TEST2">>, <<"val2">>},
                                            {<<"TEST3">>, <<"val3">>},
                                            {<<"TEst4">>, <<"val4">>}],
                                           <<"">>},
                                          Opts)
                       end),
         ?_assertMatch({fresh, _},
                       begin
                           VaryMany(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"tESt1">>, <<"val1">>},
                                            {<<"tESt2">>, <<"val2">>},
                                            {<<"tESt3">>, <<"val3">>}],
                                           <<"">>},
                                          Opts)
                       end),
         ?_assertMatch({fresh, _},
                       begin
                           VaryMany(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"tesT3">>, <<"val3">>},
                                            {<<"tesT1">>, <<"val1">>},
                                            {<<"tesT2">>, <<"val2">>}],
                                           <<"">>},
                                          Opts)
                       end),
         ?_assertMatch(miss,
                       begin
                           VaryMany(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"TESt2">>, <<"val2">>}, {<<"TEst3">>, <<"val3">>}],
                                           <<"">>},
                                          Opts)
                       end),
         ?_assertMatch({fresh, _},
                       begin
                           VaryNorm(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"tesT">>, <<"val1">>},
                                            {<<"teST">>, <<"   val2  ">>}],
                                           <<"">>},
                                          Opts)
                       end),
         ?_assertMatch({fresh, _},
                       begin
                           VaryNorm(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"TESt">>, <<"val1, val2">>}],
                                           <<"">>},
                                          Opts)
                       end),
         ?_assertMatch(miss,
                       begin
                           VaryNorm(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"TeSt">>, <<"val1">>}],
                                           <<"">>},
                                          Opts)
                       end),
         ?_assertMatch(miss,
                       begin
                           VaryNorm(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"TesT">>, <<"val2">>}],
                                           <<"">>},
                                          Opts)
                       end)],
    [{spawn, Test} || Test <- Tests].

rfc7234_section_4_2_stale_on_expired(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [{<<"cache-control">>, <<"max-stale=10">>}], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"expires">>, timestamp_to_rfc7231(unix_now() - 1)}],
                             <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({stale, _},
                   begin
                       F(),
                       http_cache:get(Req, Opts)
                   end)}.

rfc7234_section_4_2_1_smaxage_shared(Opts) ->
    Store = proplists:get_value(store, Opts),
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"s-maxage=3, max-age=5">>},
                              {<<"expires">>, timestamp_to_rfc7231(unix_now() + 7)}],
                             <<"Some content">>},
                            Opts),
           {fresh, {RespRef, _}} = http_cache:get(Req, Opts),
           {_, _, _, #{expires := Expires}} = Store:get_response(RespRef),
           Expires
        end,
    {spawn, ?_assertEqual(unix_now() + 3, F())}.

rfc7234_section_4_2_1_smaxage_private(Opts) ->
    Store = proplists:get_value(store, Opts),
    OptsPriv = set_opt(type, private, Opts),
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"s-maxage=3, max-age=5">>},
                              {<<"expires">>, timestamp_to_rfc7231(unix_now() + 7)}],
                             <<"Some content">>},
                            OptsPriv),
           {fresh, {RespRef, _}} = http_cache:get(Req, Opts),
           {_, _, _, #{expires := Expires}} = Store:get_response(RespRef),
           Expires
        end,
    {spawn, ?_assertEqual(unix_now() + 5, F())}.

rfc7234_section_4_2_1_maxage(Opts) ->
    Store = proplists:get_value(store, Opts),
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=5">>},
                              {<<"expires">>, timestamp_to_rfc7231(unix_now() + 7)}],
                             <<"Some content">>},
                            Opts),
           {fresh, {RespRef, _}} = http_cache:get(Req, Opts),
           {_, _, _, #{expires := Expires}} = Store:get_response(RespRef),
           Expires
        end,
    {spawn, ?_assertEqual(unix_now() + 5, F())}.

rfc7234_section_4_2_1_expires(Opts) ->
    Store = proplists:get_value(store, Opts),
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"expires">>, timestamp_to_rfc7231(unix_now() + 7)}],
                             <<"Some content">>},
                            Opts),
           {fresh, {RespRef, _}} = http_cache:get(Req, Opts),
           {_, _, _, #{expires := Expires}} = Store:get_response(RespRef),
           Expires
        end,
    {spawn, ?_assertEqual(unix_now() + 7, F())}.

rfc7234_section_4_2_2_heuristics_no_used(Opts) ->
    Store = proplists:get_value(store, Opts),
    TTL = proplists:get_value(default_ttl, Opts),
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"expires">>, timestamp_to_rfc7231(unix_now() + 1337)}],
                             <<"Some content">>},
                            Opts),
           {fresh, {RespRef, _}} = http_cache:get(Req, Opts),
           {_, _, _, #{expires := Expires}} = Store:get_response(RespRef),
           Expires
        end,
    {spawn, ?_assertNotEqual(unix_now() + TTL, F())}.

rfc7234_section_4_2_3_age_no_date_header(Opts) ->
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req, {200, [], <<"Some content">>}, Opts),
           {fresh, {_RespRef, {_, RespHeaders, _}}} = http_cache:get(Req, Opts),
           proplists:get_value(<<"age">>, RespHeaders)
        end,
    {spawn, ?_assertEqual(F(), <<"0">>)}.

rfc7234_section_4_2_3_age_with_date_header(Opts) ->
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           TwoMinutesAgo = timestamp_to_rfc7231(unix_now() - 2 * 60),
           http_cache:cache(Req, {200, [{<<"date">>, TwoMinutesAgo}], <<"Some content">>}, Opts),
           {fresh, {_RespRef, {_, RespHeaders, _}}} = http_cache:get(Req, Opts),
           proplists:get_value(<<"age">>, RespHeaders)
        end,
    {spawn, ?_assertEqual(F(), <<"120">>)}.

rfc7234_section_4_2_4_no_stale_returned_resp_ccdir_no_cache(Opts) ->
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"no-cache">>}], <<"Some content">>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn, ?_assertMatch({must_revalidate, _}, F())}.

rfc7234_section_4_2_4_no_stale_returned_resp_ccdir_no_store(Opts) ->
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0">>},
                              {<<"cache-control">>, <<"no-store">>}],
                             <<"Some content">>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn, ?_assertEqual(miss, F())}.

rfc7234_section_4_2_4_no_stale_returned_resp_ccdir_must_revalidate(Opts) ->
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0">>},
                              {<<"cache-control">>, <<"must-revalidate">>}],
                             <<"Some content">>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn, ?_assertMatch({must_revalidate, _}, F())}.

rfc7234_section_4_2_4_stale_returned_resp_ccdir_proxy_revalidate_priv_cache(Opts) ->
    OptsPriv = set_opt(type, private, Opts),
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"proxy-revalidate">>}],
                             <<"Some content">>},
                            OptsPriv),
           http_cache:get(Req, OptsPriv)
        end,
    {spawn, ?_assertMatch({fresh, _}, F())}.

rfc7234_section_4_2_4_no_stale_returned_resp_ccdir_proxy_revalidate_shared_cache(Opts) ->
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0">>},
                              {<<"cache-control">>, <<"proxy-revalidate">>}],
                             <<"Some content">>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn, ?_assertMatch({must_revalidate, _}, F())}.

rfc7234_section_4_3_2_if_none_match_strong_etag(Opts) ->
    F = fun(Method, ReqHeaders) ->
           http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                            {200, [{<<"etag">>, <<"\"some-etag\"">>}], <<"Some content">>},
                            Opts),
           http_cache:get({Method, ?TEST_URL, ReqHeaders, <<"">>}, Opts)
        end,
    [[{spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [{<<"if-none-match">>, <<"\"some-etag\"">>}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [{<<"if-none-match">>, <<"W/\"some-etag\"">>}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}}, F(Method, [{<<"if-none-match">>, <<"*">>}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method,
                       [{<<"if-none-match">>,
                         <<"\"another-etag\", \"some-etag\", \"one-last\"">>}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {200, _, _}}},
                     F(Method, [{<<"if-none-match">>, <<"\"another-etag\"">>}]))}]
     || Method <- [<<"GET">>, <<"HEAD">>]].

rfc7234_section_4_3_2_if_none_match_weak_etag(Opts) ->
    F = fun(Method, ReqHeaders) ->
           http_cache:cache({Method, ?TEST_URL, [], <<"">>},
                            {200, [{<<"etag">>, <<"W/\"some-etag\"">>}], <<"Some content">>},
                            Opts),
           http_cache:get({Method, ?TEST_URL, ReqHeaders, <<"">>}, Opts)
        end,
    [[{spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [{<<"if-none-match">>, <<"\"some-etag\"">>}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [{<<"if-none-match">>, <<"W/\"some-etag\"">>}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}}, F(Method, [{<<"if-none-match">>, <<"*">>}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method,
                       [{<<"if-none-match">>,
                         <<"W/\"another-etag\", W/\"some-etag\", W/\"one-last\"">>}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {200, _, _}}},
                     F(Method, [{<<"if-none-match">>, <<"W/\"another-etag\"">>}]))}]
     || Method <- [<<"GET">>, <<"HEAD">>]].

rfc7234_section_4_3_2_if_none_match_precondition_failed_for_side_effect_methods(Opts) ->
    F = fun() ->
           Req = {<<"POST">>, ?TEST_URL, [{<<"if-none-match">>, <<"\"some-etag\"">>}], <<"">>},
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=60">>},
                              {<<"etag">>, <<"\"some-etag\"">>}],
                             <<"Some content">>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn, ?_assertMatch({fresh, {_, {412, _, _}}}, F())}.

rfc7234_section_4_3_2_if_modified_since(Opts) ->
    Past = timestamp_to_rfc7231(unix_now() - 10),
    Now = timestamp_to_rfc7231(unix_now()),
    Future = timestamp_to_rfc7231(unix_now() + 10),
    F = fun(Method, RespHeaders, ReqHeaders) ->
           http_cache:cache({Method, ?TEST_URL, [], <<>>},
                            {200, RespHeaders, <<"Some content">>},
                            Opts),
           http_cache:get({Method, ?TEST_URL, ReqHeaders, <<"">>}, Opts)
        end,
    [[{spawn,
       ?_assertMatch({fresh, {_, {200, _, _}}},
                     F(Method, [{<<"last-modified">>, Now}], [{<<"if-modified-since">>, Past}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [{<<"last-modified">>, Now}], [{<<"if-modified-since">>, Now}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [{<<"last-modified">>, Now}], [{<<"if-modified-since">>, Future}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {200, _, _}}},
                     F(Method, [{<<"date">>, Now}], [{<<"if-modified-since">>, Past}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [{<<"date">>, Now}], [{<<"if-modified-since">>, Now}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [{<<"date">>, Now}], [{<<"if-modified-since">>, Future}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {200, _, _}}},
                     F(Method, [], [{<<"if-modified-since">>, Past}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [], [{<<"if-modified-since">>, Now}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method, [], [{<<"if-modified-since">>, Future}]))}]
     || Method <- [<<"GET">>, <<"HEAD">>]].

rfc7234_section_4_3_2_if_none_match_has_precedence_over_if_modified_since(Opts) ->
    Now = timestamp_to_rfc7231(unix_now()),
    Future = timestamp_to_rfc7231(unix_now() + 10),
    F = fun(Method, ReqHeaders, RespHeaders) ->
           http_cache:cache({Method, ?TEST_URL, [], <<>>},
                            {200, RespHeaders, <<"Some content">>},
                            Opts),
           http_cache:get({Method, ?TEST_URL, ReqHeaders, <<>>}, Opts)
        end,
    [[{spawn,
       ?_assertMatch({fresh, {_, {200, _, _}}},
                     F(Method,
                       [{<<"if-none-match">>, <<"\"some-other-etag\"">>},
                        {<<"if-modified-since">>, Future}],
                       [{<<"etag">>, <<"\"some-etag\"">>}, {<<"last-modified">>, Now}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {200, _, _}}},
                     F(Method,
                       [{<<"if-none-match">>, <<"\"some-other-etag\"">>},
                        {<<"if-modified-since">>, Future}],
                       [{<<"last-modified">>, Now}]))},
      {spawn,
       ?_assertMatch({fresh, {_, {304, _, _}}},
                     F(Method,
                       [{<<"if-modified-since">>, Future}],
                       [{<<"etag">>, <<"\"some-etag\"">>}, {<<"last-modified">>, Now}]))}]
     || Method <- [<<"GET">>, <<"HEAD">>]].

rfc7234_section_4_3_2_if_range_with_etag(Opts) ->
    F = fun(ReqHeaders, RespHeaders) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<>>},
                            {200, RespHeaders, <<"Some content">>},
                            Opts),
           http_cache:get({<<"GET">>,
                           ?TEST_URL,
                           [{<<"range">>, <<"bytes=0-3">>} | ReqHeaders],
                           <<>>},
                          Opts)
        end,
    [{spawn,
      ?_assertMatch({fresh, {_, {206, _, _}}},
                    F([{<<"if-range">>, <<"\"some-etag\"">>}],
                      [{<<"etag">>, <<"\"some-etag\"">>}]))},
     {spawn,
      ?_assertMatch({fresh, {_, {200, _, _}}},
                    F([{<<"if-range">>, <<"W/\"some-etag\"">>}],
                      [{<<"etag">>, <<"\"some-etag\"">>}]))},
     {spawn,
      ?_assertMatch({fresh, {_, {200, _, _}}},
                    F([{<<"if-range">>, <<"W/\"some-etag\"">>}],
                      [{<<"etag">>, <<"W/\"some-etag\"">>}]))},
     {spawn,
      ?_assertMatch({fresh, {_, {200, _, _}}},
                    F([{<<"if-range">>, <<"W/\"some-etag\"">>}], []))}].

rfc7234_section_4_3_2_if_range_with_date(Opts) ->
    OneAndAHalfMinuteAgo = timestamp_to_rfc7231(unix_now() - 90),
    HalfMinuteAgo = timestamp_to_rfc7231(unix_now() - 30),
    Now = timestamp_to_rfc7231(unix_now()),
    F = fun(ReqHeaders, RespHeaders) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<>>},
                            {200, RespHeaders, <<"Some content">>},
                            Opts),
           http_cache:get({<<"GET">>,
                           ?TEST_URL,
                           [{<<"range">>, <<"bytes=0-3">>} | ReqHeaders],
                           <<>>},
                          Opts)
        end,
    [{spawn,
      ?_assertMatch({fresh, {_, {206, _, _}}},
                    F([{<<"if-range">>, Now}],
                      [{<<"last-modified">>, OneAndAHalfMinuteAgo}, {<<"date">>, Now}]))},
     {spawn,
      ?_assertMatch({fresh, {_, {200, _, _}}},
                    F([{<<"if-range">>, Now}],
                      [{<<"last-modified">>, HalfMinuteAgo}, {<<"date">>, Now}]))},
     {spawn,
      ?_assertMatch({fresh, {_, {200, _, _}}},
                    F([{<<"if-range">>, Now}], [{<<"last-modified">>, HalfMinuteAgo}]))},
     {spawn,
      ?_assertMatch({fresh, {_, {200, _, _}}},
                    F([{<<"if-range">>, Now}], [{<<"date">>, Now}]))},
     {spawn, ?_assertMatch({fresh, {_, {200, _, _}}}, F([{<<"if-range">>, Now}], []))}].

rfc7234_section_4_3_4_cached_response_updated_with_strong_validator_etag(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"test-header">>, <<"a1">>},
                              {<<"etag">>, <<"\"strong-etag\"">>},
                              {<<"test-header">>, <<"a2">>}],
                             <<"Some content">>},
                            set_opt(default_ttl, 0, Opts)),
           http_cache:cache(Req,
                            {304,
                             [{<<"etag">>, <<"\"strong-etag\"">>}, {<<"test-header">>, <<"b">>}],
                             <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"b">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"0">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"age">>, RespHeaders, undefined)
                    end)}].

rfc7234_section_4_3_4_cached_response_updated_with_strong_validator_etag_revalidation(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0">>},
                              {<<"test-header">>, <<"a1">>},
                              {<<"etag">>, <<"\"strong-etag\"">>},
                              {<<"test-header">>, <<"a2">>}],
                             <<"Some content">>},
                            Opts),
           {must_revalidate, {_, RespToRevalidate}} = http_cache:get(Req, Opts),
           http_cache:cache(Req,
                            {304,
                             [{<<"etag">>, <<"\"strong-etag\"">>}, {<<"test-header">>, <<"b">>}],
                             <<>>},
                            RespToRevalidate,
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"b">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"0">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"age">>, RespHeaders, undefined)
                    end)}].

rfc7234_section_4_3_4_cached_response_updated_with_weak_validator_last_modified(Opts) ->
    OneMinuteAgo = timestamp_to_rfc7231(unix_now() - 60),
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"last-modified">>, OneMinuteAgo}, {<<"most-recent">>, <<"nope">>}],
                             <<"Some content">>},
                            set_opt(request_time, unix_now() - 60, Opts)),
           http_cache:cache(Req,
                            {200,
                             [{<<"last-modified">>, OneMinuteAgo}, {<<"most-recent">>, <<"yeah">>}],
                             <<"Some content">>},
                            Opts),
           http_cache:cache(Req,
                            {304,
                             [{<<"last-modified">>, OneMinuteAgo}, {<<"test-header">>, <<"a">>}],
                             <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"yeah">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"most-recent">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"a">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"0">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"age">>, RespHeaders, undefined)
                    end)}].

rfc7234_section_4_3_4_cached_response_updated_with_weak_validator_last_modified_revalidation(Opts) ->
    OneMinuteAgo = timestamp_to_rfc7231(unix_now() - 60),
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0">>},
                              {<<"last-modified">>, OneMinuteAgo},
                              {<<"most-recent">>, <<"nope">>}],
                             <<"Some content">>},
                            set_opt(request_time, unix_now() - 60, Opts)),
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0">>},
                              {<<"last-modified">>, OneMinuteAgo},
                              {<<"most-recent">>, <<"yeah">>}],
                             <<"Some content">>},
                            Opts),
           {must_revalidate, {_, RespToRevalidate}} = http_cache:get(Req, Opts),
           http_cache:cache(Req,
                            {304,
                             [{<<"last-modified">>, OneMinuteAgo}, {<<"test-header">>, <<"a">>}],
                             <<>>},
                            RespToRevalidate,
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"yeah">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"most-recent">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"a">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"0">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"age">>, RespHeaders, undefined)
                    end)}].

rfc7234_section_4_3_4_cached_response_updated_with_weak_validator_etag(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"etag">>, <<"W/\"weak-etag\"">>}, {<<"most-recent">>, <<"nope">>}],
                             <<"Some content">>},
                            set_opt(request_time, unix_now() - 60, Opts)),
           http_cache:cache(Req,
                            {200,
                             [{<<"etag">>, <<"W/\"weak-etag\"">>}, {<<"most-recent">>, <<"yeah">>}],
                             <<"Some content">>},
                            set_opt(request_time, unix_now() - 30, Opts)),
           http_cache:cache(Req,
                            {304,
                             [{<<"etag">>, <<"W/\"weak-etag\"">>}, {<<"test-header">>, <<"a">>}],
                             <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"yeah">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"most-recent">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"a">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"0">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"age">>, RespHeaders, undefined)
                    end)}].

rfc7234_section_4_3_4_cached_response_updated_with_weak_validator_etag_revalidation(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0">>},
                              {<<"etag">>, <<"W/\"weak-etag\"">>},
                              {<<"most-recent">>, <<"nope">>}],
                             <<"Some content">>},
                            set_opt(request_time, unix_now() - 60, Opts)),
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0">>},
                              {<<"etag">>, <<"W/\"weak-etag\"">>},
                              {<<"most-recent">>, <<"yeah">>}],
                             <<"Some content">>},
                            set_opt(request_time, unix_now() - 30, Opts)),
           {must_revalidate, {_, RespToRevalidate}} = http_cache:get(Req, Opts),
           http_cache:cache(Req,
                            {304,
                             [{<<"etag">>, <<"W/\"weak-etag\"">>}, {<<"test-header">>, <<"a">>}],
                             <<>>},
                            RespToRevalidate,
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"yeah">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"most-recent">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"a">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"0">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"age">>, RespHeaders, undefined)
                    end)}].

rfc7234_section_4_3_4_cached_response_updated_with_no_validator(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun(RespHeaders) ->
           http_cache:cache(Req, {200, RespHeaders, <<"Some content">>}, Opts),
           http_cache:cache(Req, {304, [{<<"test-header">>, <<"a">>}], <<>>}, Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"a">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F([]),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertNotEqual(<<"a">>,
                       begin
                           {_, {_, {_, RespHeaders, _}}} = F([{<<"etag">>, <<"\"strong-etag\"">>}]),
                           proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                       end)},
     {spawn,
      ?_assertNotEqual(<<"a">>,
                       begin
                           {_, {_, {_, RespHeaders, _}}} =
                               F([{<<"last-modified">>, timestamp_to_rfc7231(unix_now())}]),
                           proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                       end)}].

rfc7234_section_4_3_4_cached_response_updated_with_no_validator_revalidation(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun(RespHeaders) ->
           http_cache:cache(Req,
                            {200,
                             RespHeaders ++ [{<<"cache-control">>, <<"max-age=0">>}],
                             <<"Some content">>},
                            Opts),
           {must_revalidate, {_, RespToRevalidate}} = http_cache:get(Req, Opts),
           http_cache:cache(Req,
                            {304, [{<<"test-header">>, <<"a">>}], <<>>},
                            RespToRevalidate,
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"a">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F([]),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertNotEqual(<<"a">>,
                       begin
                           {_, {_, {_, RespHeaders, _}}} = F([{<<"etag">>, <<"\"strong-etag\"">>}]),
                           proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                       end)},
     {spawn,
      ?_assertNotEqual(<<"a">>,
                       begin
                           {_, {_, {_, RespHeaders, _}}} =
                               F([{<<"last-modified">>, timestamp_to_rfc7231(unix_now())}]),
                           proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                       end)}].

rfc7234_section_4_3_5_cached_response_updated_with_etag_matching(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200, [{<<"etag">>, <<"\"strong-etag\"">>}], <<"Some content">>},
                            Opts),
           http_cache:cache({<<"HEAD">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"etag">>, <<"\"strong-etag\"">>}, {<<"test-header">>, <<"a">>}],
                             <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"a">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"0">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"age">>, RespHeaders, undefined)
                    end)}].

rfc7234_section_4_3_5_cached_response_updated_with_etag_matching_and_content_length(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"etag">>, <<"\"strong-etag\"">>},
                              {<<"content-length">>, <<"12">>}],
                             <<"Some content">>},
                            Opts),
           http_cache:cache({<<"HEAD">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"etag">>, <<"\"strong-etag\"">>},
                              {<<"content-length">>, <<"12">>},
                              {<<"test-header">>, <<"a">>}],
                             <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"a">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"0">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"age">>, RespHeaders, undefined)
                    end)}].

rfc7234_section_4_3_5_cached_response_updated_with_last_modified_matching(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    LastModified = timestamp_to_rfc7231(unix_now()),
    F = fun() ->
           http_cache:cache(Req,
                            {200, [{<<"last-modified">>, LastModified}], <<"Some content">>},
                            Opts),
           http_cache:cache({<<"HEAD">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"last-modified">>, LastModified}, {<<"test-header">>, <<"a">>}],
                             <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn,
      ?_assertEqual(<<"a">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                    end)},
     {spawn,
      ?_assertEqual(<<"0">>,
                    begin
                        {_, {_, {_, RespHeaders, _}}} = F(),
                        proplists:get_value(<<"age">>, RespHeaders, undefined)
                    end)}].

rfc7234_section_4_3_5_cached_response_updated_with_last_modified_matching_and_content_length(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    LastModified = timestamp_to_rfc7231(unix_now()),
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"last-modified">>, LastModified},
                              {<<"content-length">>, <<"12">>}],
                             <<"Some content">>},
                            Opts),
           http_cache:cache({<<"HEAD">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"last-modified">>, LastModified},
                              {<<"content-length">>, <<"12">>},
                              {<<"test-header">>, <<"a">>}],
                             <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn,
     ?_assertEqual(<<"a">>,
                   begin
                       {_, {_, {_, RespHeaders, _}}} = F(),
                       proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                   end)}.

rfc7234_section_4_3_5_cached_response_invalidated_no_validator(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req, {200, [], <<"Some content">>}, Opts),
           http_cache:cache({<<"HEAD">>, ?TEST_URL, [{<<"test-header">>, <<"a">>}], <<"">>},
                            {200, [], <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn,
     ?_assertNotMatch(<<"a">>,
                      begin
                          {must_revalidate, {_, {_, RespHeaders, _}}} = F(),
                          proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                      end)}.

rfc7234_section_4_3_5_cached_response_invalidated_no_validator_get_resp(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req, {200, [], <<"Some content">>}, Opts),
           http_cache:cache({<<"HEAD">>,
                             ?TEST_URL,
                             [{<<"etag">>, <<"\"strong-etag\"">>},
                              {<<"last-modified">>, timestamp_to_rfc7231(unix_now())},
                              {<<"test-header">>, <<"a">>}],
                             <<"">>},
                            {200, [], <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn,
     ?_assertNotMatch(<<"a">>,
                      begin
                          {must_revalidate, {_, {_, RespHeaders, _}}} = F(),
                          proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                      end)}.

rfc7234_section_4_3_5_cached_response_invalidated_no_validator_head_resp(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"etag">>, <<"\"strong-etag\"">>},
                              {<<"last-modified">>, timestamp_to_rfc7231(unix_now())}],
                             <<"Some content">>},
                            Opts),
           http_cache:cache({<<"HEAD">>, ?TEST_URL, [{<<"test-header">>, <<"a">>}], <<"">>},
                            {200, [], <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn,
     ?_assertNotMatch(<<"a">>,
                      begin
                          {must_revalidate, {_, {_, RespHeaders, _}}} = F(),
                          proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                      end)}.

rfc7234_section_4_3_5_cached_response_invalidated_content_length_mismatch(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"etag">>, <<"\"strong-etag\"">>}, {<<"content-length">>, <<"3">>}],
                             <<"abc">>},
                            Opts),
           http_cache:cache({<<"HEAD">>,
                             ?TEST_URL,
                             [{<<"etag">>, <<"\"strong-etag\"">>},
                              {<<"content-length">>, <<"4">>},
                              {<<"test-header">>, <<"a">>}],
                             <<"">>},
                            {200, [], <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    {spawn,
     ?_assertNotMatch(<<"a">>,
                      begin
                          {must_revalidate, {_, {_, RespHeaders, _}}} = F(),
                          proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                      end)}.

rfc7234_section_4_3_5_ignore_when_status_code_is_not_200(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    F = fun() ->
           http_cache:cache(Req,
                            {200, [{<<"etag">>, <<"\"strong-etag\"">>}], <<"Some content">>},
                            Opts),
           http_cache:cache({<<"HEAD">>, ?TEST_URL, [], <<"">>},
                            {404,
                             [{<<"etag">>, <<"\"strong-etag\"">>}, {<<"test-header">>, <<"a">>}],
                             <<>>},
                            Opts),
           http_cache:get(Req, Opts)
        end,
    [{spawn, ?_assertMatch({fresh, _}, F())},
     {spawn,
      ?_assertNotEqual(<<"a">>,
                       begin
                           {_, {_, {_, RespHeaders, _}}} = F(),
                           proplists:get_value(<<"test-header">>, RespHeaders, undefined)
                       end)}].

rfc7234_section_4_4_invalidate_uri_of_unsafe_method_on_non_error_status(Opts) ->
    URI = ?TEST_URL,
    OtherURI = <<"http://example.com/somewhere/else">>,
    [begin
         F = fun() ->
                http_cache:cache({<<"GET">>, URI, [], <<"">>}, {200, [], <<"Some content">>}, Opts),
                case RespHeaders of
                    [] ->
                        ok;
                    _ ->
                        http_cache:cache({<<"GET">>, OtherURI, [], <<"">>},
                                         {200, [], <<"Some content">>},
                                         Opts)
                end,
                not_cacheable =
                    http_cache:cache({UnsafeMethod, URI, [], <<"">>},
                                     {SuccessStatus, RespHeaders, <<"">>},
                                     Opts),
                {http_cache:get({<<"GET">>, URI, [], <<"">>}, Opts),
                 http_cache:get({<<"GET">>, OtherURI, [], <<"">>}, Opts)}
             end,
         ?_assertMatch({miss, miss}, F())
     end
     || RespHeaders
            <- [[], [{<<"location">>, OtherURI}], [{<<"content-location">>, OtherURI}]],
        UnsafeMethod <- [<<"PUT">>, <<"POST">>, <<"PATCH">>, <<"DELETE">>, <<"UNKNOWN">>],
        SuccessStatus <- [Status || Status <- ?ALL_STATUSES, Status >= 200, Status < 400]].

rfc7234_section_4_4_no_invalidate_uri_of_unsafe_method_on_error_status(Opts) ->
    URI = ?TEST_URL,
    OtherURI = <<"http://example.com/somewhere/else">>,
    [begin
         F = fun() ->
                http_cache:cache({<<"GET">>, URI, [], <<"">>}, {200, [], <<"Some content">>}, Opts),
                http_cache:cache({<<"GET">>, OtherURI, [], <<"">>},
                                 {200, [], <<"Some content">>},
                                 Opts),
                not_cacheable =
                    http_cache:cache({UnsafeMethod, URI, [], <<"">>},
                                     {SuccessStatus, RespHeaders, <<"">>},
                                     Opts),
                {http_cache:get({<<"GET">>, URI, [], <<"">>}, Opts),
                 http_cache:get({<<"GET">>, OtherURI, [], <<"">>}, Opts)}
             end,
         ?_assertMatch({{fresh, _}, {fresh, _}}, F())
     end
     || RespHeaders
            <- [[], [{<<"location">>, OtherURI}], [{<<"content-location">>, OtherURI}]],
        UnsafeMethod <- [<<"PUT">>, <<"POST">>, <<"PATCH">>, <<"DELETE">>, <<"UNKNOWN">>],
        SuccessStatus <- [Status || Status <- ?ALL_STATUSES, Status > 400]].

rfc7234_section_5_2_1_1_ccdir_max_age(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=60">>}], <<"Some content">>},
                            Opts)
        end,
    [{spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(),
                        http_cache:get({<<"GET">>,
                                        ?TEST_URL,
                                        [{<<"cache-control">>, <<"max-age=5">>}],
                                        <<"">>},
                                       Opts)
                    end)},
     {spawn,
      ?_assertNotMatch({fresh, _},
                       begin
                           Store(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"cache-control">>, <<"max-age=0">>}],
                                           <<"">>},
                                          Opts)
                       end)}].

rfc7234_section_5_2_1_2_ccdir_max_stale(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=0">>}], <<"Some content">>},
                            Opts)
        end,
    [{spawn,
      ?_assertMatch({stale, _},
                    begin
                        Store(),
                        http_cache:get({<<"GET">>,
                                        ?TEST_URL,
                                        [{<<"cache-control">>, <<"max-stale=5">>}],
                                        <<"">>},
                                       Opts)
                    end)},
     {spawn,
      ?_assertNotMatch({stale, _},
                       begin
                           Store(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"cache-control">>, <<"max-stale=0">>}],
                                           <<"">>},
                                          Opts)
                       end)}].

rfc7234_section_5_2_1_3_ccdir_min_fresh(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=60">>}], <<"Some content">>},
                            Opts)
        end,
    [{spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(),
                        http_cache:get({<<"GET">>,
                                        ?TEST_URL,
                                        [{<<"cache-control">>, <<"min-fresh=50">>}],
                                        <<"">>},
                                       Opts)
                    end)},
     {spawn,
      ?_assertNotMatch({fresh, _},
                       begin
                           Store(),
                           http_cache:get({<<"GET">>,
                                           ?TEST_URL,
                                           [{<<"cache-control">>, <<"min-fresh=70">>}],
                                           <<"">>},
                                          Opts)
                       end)}].

rfc7234_section_5_2_1_4_ccdir_no_cache(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=60">>}], <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({must_revalidate, _},
                   begin
                       Store(),
                       http_cache:get({<<"GET">>,
                                       ?TEST_URL,
                                       [{<<"cache-control">>, <<"no-cache">>}],
                                       <<"">>},
                                      Opts)
                   end)}.

rfc7234_section_5_2_1_5_ccdir_no_store(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [{<<"cache-control">>, <<"no-store">>}], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=60">>}], <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch(miss,
                   begin
                       Store(),
                       http_cache:get({<<"GET">>, ?TEST_URL, [], <<"">>}, Opts)
                   end)}.

rfc7234_section_5_2_1_7_ccdir_only_if_cached(Opts) ->
    F = fun(RespHeaders) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, RespHeaders, <<"Some content">>},
                            Opts),
           http_cache:get({<<"GET">>,
                           ?TEST_URL,
                           [{<<"cache-control">>, <<"only-if-cached">>}],
                           <<"">>},
                          Opts)
        end,
    [{spawn, ?_assertMatch({fresh, {_, {200, _, _}}}, F([]))},
     {spawn,
      ?_assertMatch({fresh, {_, {504, _, _}}}, F([{<<"cache-control">>, <<"max-age=0">>}]))},
     {spawn,
      ?_assertMatch({fresh, {_, {504, _, _}}},
                    http_cache:get({<<"GET">>,
                                    <<"http://example.com/not_cached">>,
                                    [{<<"cache-control">>, <<"only-if-cached">>}],
                                    <<"">>},
                                   Opts))}].

rfc7234_section_5_2_2_1_ccdir_must_revalidate(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0, must-revalidate">>}],
                             <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({must_revalidate, _},
                   begin
                       Store(),
                       http_cache:get(Req, Opts)
                   end)}.

rfc7234_section_5_2_2_2_ccdir_no_cache(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"no-cache">>}], <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({must_revalidate, _},
                   begin
                       Store(),
                       http_cache:get(Req, Opts)
                   end)}.

rfc7234_section_5_2_2_3_ccdir_no_store(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"no-store">>}], <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch(miss,
                   begin
                       Store(),
                       http_cache:get(Req, Opts)
                   end)}.

rfc7234_section_5_2_2_4_ccdir_no_transform_auto_compress(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Resp =
        {200,
         [{<<"content-type">>, <<"text/plain">>}, {<<"cache-control">>, <<"no-transform">>}],
         <<"Some content">>},
    Store = fun() -> http_cache:cache(Req, Resp, set_opt(auto_compress, true, Opts)) end,
    [{spawn,
      ?_assertMatch({fresh, {_, {200, _, <<"Some content">>}}},
                    begin
                        Store(),
                        http_cache:get({<<"GET">>,
                                        ?TEST_URL,
                                        [{<<"accept-encoding">>, <<"gzip">>}],
                                        <<"">>},
                                       set_opt(auto_decompress, true, Opts))
                    end)}].

rfc7234_section_5_2_2_4_ccdir_no_transform_auto_decompress(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Resp =
        {200,
         [{<<"content-encoding">>, <<"gzip">>},
          {<<"cache-control">>, <<"no-transform">>},
          {<<"vary">>, <<"accept-encoding">>}],
         zlib:gzip(<<"Some compressed content">>)},
    Store = fun() -> http_cache:cache(Req, Resp, set_opt(auto_decompress, true, Opts)) end,
    [{spawn,
      ?_assertMatch(miss,
                    begin
                        Store(),
                        http_cache:get({<<"GET">>, ?TEST_URL, [], <<"">>},
                                       set_opt(auto_decompress, true, Opts))
                    end)},
     {spawn,
      ?_assertMatch(miss,
                    begin
                        Store(),
                        http_cache:get({<<"GET">>,
                                        ?TEST_URL,
                                        [{<<"accept-encoding">>, <<"identity">>}],
                                        <<"">>},
                                       set_opt(auto_decompress, true, Opts))
                    end)}].

rfc7234_section_5_2_2_4_ccdir_no_transform_range(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Resp = {200, [{<<"cache-control">>, <<"no-transform">>}], <<"Some content">>},
    Store = fun() -> http_cache:cache(Req, Resp, Opts) end,
    {spawn,
     ?_assertMatch({fresh, {_, {200, _, _}}},
                   begin
                       Store(),
                       http_cache:get({<<"GET">>,
                                       ?TEST_URL,
                                       [{<<"range">>, <<"bytes=1-3">>}],
                                       <<"">>},
                                      Opts)
                   end)}.

rfc7234_section_5_2_2_5_ccdir_public(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {500, [{<<"cache-control">>, <<"public">>}], <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({fresh, _},
                   begin
                       Store(),
                       http_cache:get(Req, Opts)
                   end)}.

rfc7234_section_5_2_2_6_ccdir_private(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    PrivOpts = set_opt(type, private, Opts),
    Store =
        fun(SelectedOpts) ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"private">>}], <<"Some content">>},
                            SelectedOpts)
        end,
    [{spawn,
      ?_assertMatch(miss,
                    begin
                        Store(Opts),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(PrivOpts),
                        http_cache:get(Req, PrivOpts)
                    end)}].

rfc7234_section_5_2_2_7_ccdir_proxy_revalidate(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [{<<"cache-control">>, <<"max-stale=5">>}], <<"">>},
    PrivOpts = set_opt(type, private, Opts),
    Store =
        fun(SelectedOpts) ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0, proxy-revalidate">>}],
                             <<"Some content">>},
                            SelectedOpts)
        end,
    [{spawn,
      ?_assertMatch({must_revalidate, _},
                    begin
                        Store(Opts),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertMatch({stale, _},
                    begin
                        Store(PrivOpts),
                        http_cache:get(Req, PrivOpts)
                    end)}].

rfc7234_section_5_2_2_8_ccdir_max_age(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun(MaxAge) ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=", MaxAge/binary>>}],
                             <<"Some content">>},
                            Opts)
        end,
    [{spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(<<"10">>),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertNotMatch({fresh, _},
                       begin
                           Store(<<"0">>),
                           http_cache:get(Req, Opts)
                       end)}].

rfc7234_section_5_2_2_9_ccdir_s_maxage(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    PrivOpts = set_opt(type, private, Opts),
    Store =
        fun(MaxAge, SelectedOpts) ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"s-maxage=", MaxAge/binary>>}],
                             <<"Some content">>},
                            SelectedOpts)
        end,
    [{spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(<<"10">>, Opts),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertNotMatch({fresh, _},
                       begin
                           Store(<<"0">>, Opts),
                           http_cache:get(Req, Opts)
                       end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(<<"10">>, PrivOpts),
                        http_cache:get(Req, PrivOpts)
                    end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store(<<"0">>, PrivOpts),
                        http_cache:get(Req, PrivOpts)
                    end)}].

rfc7234_section_5_3_header_expires_malformed(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"expires">>, <<"invalid expires header">>}],
                             <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertNotMatch({fresh, _},
                      begin
                          Store(),
                          http_cache:get(Req, Opts)
                      end)}.

rfc7234_section_5_3_header_expires(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    NowRFC7231 = timestamp_to_rfc7231(unix_now()),
    SoonRFC7231 = timestamp_to_rfc7231(unix_now() + 10),
    Store =
        fun(Headers) -> http_cache:cache(Req, {200, Headers, <<"Some content">>}, Opts) end,
    [{spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store([{<<"expires">>, SoonRFC7231}]),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertNotMatch({fresh, _},
                       begin
                           Store([{<<"expires">>, NowRFC7231}]),
                           http_cache:get(Req, Opts)
                       end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store([{<<"cache-control">>, <<"max-age=10">>},
                               {<<"expires">>, NowRFC7231}]),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store([{<<"cache-control">>, <<"s-maxage=10">>},
                               {<<"expires">>, NowRFC7231}]),
                        http_cache:get(Req, Opts)
                    end)}].

rfc7234_section_5_4_header_pragma(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun(Headers) -> http_cache:cache(Req, {200, Headers, <<"Some content">>}, Opts) end,
    [{spawn,
      ?_assertNotMatch({fresh, _},
                       begin
                           Store([{<<"pragma">>, <<"token=something no-cache garbage">>}]),
                           http_cache:get(Req, Opts)
                       end)},
     {spawn,
      ?_assertMatch({fresh, _},
                    begin
                        Store([{<<"pragma">>, <<"no-cache">>},
                               {<<"cache-control">>, <<"max-age=10">>}]),
                        http_cache:get(Req, Opts)
                    end)},
     {spawn,
      ?_assertNotMatch({fresh, _},
                       begin
                           Store([{<<"pragma">>, <<"no-cache">>},
                                  {<<"cache-control">>, <<"&%^%!^@(^^">>}]),
                           http_cache:get(Req, Opts)
                       end)}].

rfc7234_section_5_5_1_response_is_stale(Opts) ->
    F = fun() ->
           Req = {<<"GET">>, ?TEST_URL, [{<<"cache-control">>, <<"max-stale=10">>}], <<"">>},
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=0">>}], <<"Some content">>},
                            Opts),
           {stale, {_RespRef, {_, RespHeaders, _}}} = http_cache:get(Req, Opts),
           proplists:get_value(<<"warning">>, RespHeaders, <<"">>)
        end,
    {spawn, ?_assertMatch({_, _}, binary:match(F(), <<"110">>))}.

%rfc7234_section_5_5_2_warning_revalidation_failed(Opts) ->
%    F = fun() ->
%           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
%           http_cache:cache(Req,
%                            {200, [{<<"cache-control">>, <<"max-age=0">>}], <<"Some content">>},
%                            Opts),
%           {stale, {_, {_, RespHeaders, _}}} =
%               http_cache:get(Req, [{origin_unreachable, true} | Opts]),
%           proplists:get_value(<<"warning">>, RespHeaders, <<"">>)
%        end,
%    {spawn, ?_assertMatch({_, _}, binary:match(F(), <<"111">>))}.

rfc7234_section_5_5_4_warning_heuristics_expiration(Opts) ->
    F = fun() ->
           TwoDays = 60 * 60 * 24 * 2,
           FourDays = 2 * TwoDays,
           TestOpts =
               [{default_ttl, FourDays}, {request_time, unix_now() - TwoDays}]
               ++ proplists:delete(default_ttl, Opts),
           Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
           http_cache:cache(Req, {200, [{<<"age">>, <<"0">>}], <<"Some content">>}, TestOpts),
           {fresh, {_RespRef, {_, RespHeaders, _}}} = http_cache:get(Req, TestOpts),
           proplists:get_value(<<"warning">>, RespHeaders, <<"">>)
        end,
    {spawn, ?_assertMatch({_, _}, binary:match(F(), <<"113">>))}.

rfc7234_section_5_5_6_plain_compressed(Opts) ->
    F = fun(ReqHeaders) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [{<<"content-type">>, <<"text/plain">>}], <<"Some content">>},
                            set_opt(auto_compress, true, Opts)),
           {fresh, {_, {200, RespHeaders, _}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, ReqHeaders, <<"">>},
                              set_opt(auto_decompress, true, Opts)),
           proplists:get_value(<<"warning">>, RespHeaders, <<"">>)
        end,
    [{spawn,
      ?_assertMatch({_, _}, binary:match(F([{<<"accept-encoding">>, <<"gzip">>}]), <<"214">>))},
     {spawn, ?_assertMatch(nomatch, binary:match(F([]), <<"214">>))}].

rfc7234_section_5_5_6_already_compressed(Opts) ->
    F = fun(ReqHeaders) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"content-encoding">>, <<"gzip">>},
                              {<<"vary">>, <<"content-encoding">>}],
                             zlib:gzip(<<"Some content">>)},
                            Opts),
           {fresh, {_, {200, RespHeaders, _}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, ReqHeaders, <<"">>},
                              set_opt(auto_decompress, true, Opts)),
           proplists:get_value(<<"warning">>, RespHeaders, <<"">>)
        end,
    [{spawn,
      ?_assertMatch(nomatch,
                    binary:match(F([{<<"accept-encoding">>, <<"gzip">>}]), <<"214">>))},
     {spawn, ?_assertMatch({_, _}, binary:match(F([]), <<"214">>))}].

rfc5861_stale_while_revalidate_not_expired(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0, stale-while-revalidate=60">>}],
                             <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({stale, _},
                   begin
                       Store(),
                       http_cache:get(Req, [{allow_stale_while_revalidate, true} | Opts])
                   end)}.

rfc5861_stale_while_revalidate_expired(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0, stale-while-revalidate=0">>}],
                             <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({must_revalidate, _},
                   begin
                       Store(),
                       http_cache:get(Req, [{allow_stale_while_revalidate, true} | Opts])
                   end)}.

rfc5861_stale_if_error_req_not_expired(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [{<<"cache-control">>, <<"stale-if-error=60">>}], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=0">>}], <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({stale, _},
                   begin
                       Store(),
                       http_cache:get(Req, [{allow_stale_if_error, true} | Opts])
                   end)}.

rfc5861_stale_if_error_req_expired(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [{<<"cache-control">>, <<"stale-if-error=0">>}], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200, [{<<"cache-control">>, <<"max-age=0">>}], <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({must_revalidate, _},
                   begin
                       Store(),
                       http_cache:get(Req, [{allow_stale_if_error, true} | Opts])
                   end)}.

rfc5861_stale_if_error_resp_not_expired(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0, stale-if-error=60">>}],
                             <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({stale, _},
                   begin
                       Store(),
                       http_cache:get(Req, [{allow_stale_if_error, true} | Opts])
                   end)}.

rfc5861_stale_if_error_resp_expired(Opts) ->
    Req = {<<"GET">>, ?TEST_URL, [], <<"">>},
    Store =
        fun() ->
           http_cache:cache(Req,
                            {200,
                             [{<<"cache-control">>, <<"max-age=0, stale-if-error=0">>}],
                             <<"Some content">>},
                            Opts)
        end,
    {spawn,
     ?_assertMatch({must_revalidate, _},
                   begin
                       Store(),
                       http_cache:get(Req, [{allow_stale_if_error, true} | Opts])
                   end)}.

rfc7233_single_byte_range(Opts) ->
    F = fun(Range) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [], <<"Some content">>},
                            Opts),
           {fresh, {_, {206, _, RespBody}}} =
               http_cache:get({<<"GET">>,
                               ?TEST_URL,
                               [{<<"raNge">>, <<"bytes=", Range/binary>>}],
                               <<"">>},
                              Opts),
           iolist_to_binary(RespBody)
        end,
    [{spawn, ?_assertEqual(<<"Some">>, begin F(<<"0-3">>) end)},
     {spawn, ?_assertEqual(<<"content">>, begin F(<<"5-11">>) end)},
     {spawn, ?_assertEqual(<<"content">>, begin F(<<"5-">>) end)},
     {spawn, ?_assertEqual(<<"content">>, begin F(<<"-7">>) end)},
     {spawn, ?_assertEqual(<<"content">>, begin F(<<"5-11, 1337-">>) end)}].

rfc7233_single_byte_range_headers(Opts) ->
    F = fun(HeaderName) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [], <<"Some content">>},
                            Opts),
           {fresh, {_, {206, RespHeaders, _}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, [{<<"range">>, <<"bytes=-7">>}], <<"">>},
                              Opts),
           proplists:get_value(HeaderName, RespHeaders)
        end,
    [{spawn, ?_assertEqual(<<"bytes 5-11/12">>, F(<<"content-range">>))},
     {spawn, ?_assertEqual(<<"7">>, F(<<"content-length">>))}].

rfc7233_multiple_byte_range(Opts) ->
    Ranges = <<"bytes=0-3,5-,-7,5-11">>,
    F = fun() ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200,
                             [{<<"content-type">>, <<"text/plain; charset=utf-8">>}],
                             <<"Some content">>},
                            Opts),
           {fresh, {_, {206, RespHeaders, RespBody}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, [{<<"range">>, Ranges}], <<"">>}, Opts),
           parse_multipart_response(RespHeaders, RespBody)
        end,
    {spawn,
     ?_assertEqual([{[{<<"content-range">>, <<"bytes 0-3/12">>},
                      {<<"content-type">>, <<"text/plain; charset=utf-8">>}],
                     <<"Some">>},
                    {[{<<"content-range">>, <<"bytes 5-11/12">>},
                      {<<"content-type">>, <<"text/plain; charset=utf-8">>}],
                     <<"content">>},
                    {[{<<"content-range">>, <<"bytes 5-11/12">>},
                      {<<"content-type">>, <<"text/plain; charset=utf-8">>}],
                     <<"content">>},
                    {[{<<"content-range">>, <<"bytes 5-11/12">>},
                      {<<"content-type">>, <<"text/plain; charset=utf-8">>}],
                     <<"content">>}],
                   F())}.

rfc7233_unknown_range_type(Opts) ->
    F = fun(Range) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [], <<"Some content">>},
                            Opts),
           http_cache:get({<<"GET">>,
                           ?TEST_URL,
                           [{<<"raNge">>, <<"unknownunits=", Range/binary>>}],
                           <<"">>},
                          Opts)
        end,
    {spawn, ?_assertMatch({fresh, {_, {200, _, _}}}, begin F(<<"0-3">>) end)}.

rfc7233_no_satisfiable_range(Opts) ->
    F = fun(Range) ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [], <<"Some content">>},
                            Opts),
           http_cache:get({<<"GET">>,
                           ?TEST_URL,
                           [{<<"raNge">>, <<"bytes=", Range/binary>>}],
                           <<"">>},
                          Opts)
        end,
    [{spawn, ?_assertMatch({fresh, {_, {416, _, _}}}, begin F(<<"0-1337">>) end)},
     {spawn, ?_assertMatch({fresh, {_, {416, _, _}}}, begin F(<<"1337-0">>) end)},
     {spawn, ?_assertMatch({fresh, {_, {416, _, _}}}, begin F(<<"1337-">>) end)},
     {spawn,
      ?_assertMatch({fresh, {_, {416, _, _}}}, begin F(<<"1337-2000,3000-5000">>) end)}].

rfc7233_no_satisfiable_range_content_range_header(Opts) ->
    F = fun() ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [], <<"Some content">>},
                            Opts),
           {fresh, {_, {416, RespHeaders, _}}} =
               http_cache:get({<<"GET">>, ?TEST_URL, [{<<"raNge">>, <<"bytes=1337-">>}], <<"">>},
                              Opts),
           proplists:get_value(<<"content-range">>, RespHeaders)
        end,
    {spawn, ?_assertEqual(<<"bytes */12">>, begin F() end)}.

init() ->
    application:ensure_all_started(telemetry),
    [{default_ttl, 300},
     {default_grace, 300},
     {compression_threshold, 0},
     {store, http_cache_store_process},
     {type, shared}].

rfc7233_range_ignored_not_get(Opts) ->
    F = fun() ->
           http_cache:cache({<<"POST">>, ?TEST_URL, [], <<"">>},
                            {200, [{<<"cache-control">>, <<"max-age=60">>}], <<"Some content">>},
                            Opts),
           http_cache:get({<<"POST">>, ?TEST_URL, [{<<"range">>, <<"bytes=0-7">>}], <<"">>}, Opts)
        end,
    {spawn, ?_assertMatch({fresh, {_, {200, _, _}}}, begin F() end)}.

rfc7233_error_on_too_many_ranges(Opts) ->
    OptsRestrict = [{max_ranges, 1} | Opts],
    F = fun() ->
           http_cache:cache({<<"GET">>, ?TEST_URL, [], <<"">>},
                            {200, [], <<"Some content">>},
                            OptsRestrict),
           http_cache:get({<<"GET">>, ?TEST_URL, [{<<"range">>, <<"bytes=0-7,8-9">>}], <<"">>},
                          OptsRestrict)
        end,
    {spawn, ?_assertMatch({fresh, {_, {416, _, _}}}, begin F() end)}.

set_opt(OptName, OptValue, Opts) ->
    [{OptName, OptValue} | proplists:delete(OptName, Opts)].

unix_now() ->
    os:system_time(second).

timestamp_to_rfc7231(Timestamp) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Timestamp + 62167219200),
    cow_date:rfc7231(DateTime).

parse_multipart_response(Headers, Body) ->
    <<"multipart/byteranges; boundary=", Boundary/binary>> =
        proplists:get_value(<<"content-type">>, Headers),
    parse_multipart_response_body(iolist_to_binary(Body), Boundary).

parse_multipart_response_body(Body, Boundary) ->
    case cow_multipart:parse_headers(Body, Boundary) of
        {_, Headers, RestHeaders} ->
            case cow_multipart:parse_body(RestHeaders, Boundary) of
                {_, Content, RestBody} ->
                    [{lists:sort(Headers), Content}]
                    ++ parse_multipart_response_body(RestBody, Boundary);
                _ ->
                    []
            end;
        _ ->
            []
    end.
