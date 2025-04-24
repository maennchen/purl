-module(purl_test).

-include_lib("eunit/include/eunit.hrl").
-include("purl.hrl").

-if(?OTP_RELEASE >= 27).
doctest_test() ->
    doctest:module(purl, #{
        records => [{purl, record_info(fields, purl)}]
    }).
-endif.

to_uri_test() ->
    URI = purl:to_uri(#purl{type = "hex", name = "purl"}),
    ?assertEqual(<<"pkg:hex/purl">>, uri_string:recompose(URI)).
