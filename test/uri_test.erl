%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(uri_test).

-include_lib("eunit/include/eunit.hrl").

serialize_test_() ->
  [?_assertEqual(<<"//example.com">>,
                 uri:serialize(#{host => <<"example.com">>})),
   ?_assertEqual(<<"http:">>,
                 uri:serialize(#{scheme => <<"http">>})),
   ?_assertEqual(<<"http://example.com">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>})),
   ?_assertEqual(<<"http://example.com:80">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 port => 80})),
   ?_assertEqual(<<"http://127.0.0.1:80">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"127.0.0.1">>,
                                 port => 80})),
   ?_assertEqual(<<"http://[::1]:80">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"[::1]">>,
                                 port => 80})),
   ?_assertEqual(<<"http://bob@example.com">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 username => <<"bob">>})),
   ?_assertEqual(<<"http://:foo@example.com">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 password => <<"foo">>})),
   ?_assertEqual(<<"http://john%20doe:foo%20bar@example.com">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 username => <<"john doe">>,
                                 password => <<"foo bar">>})),
   ?_assertEqual(<<"http://m%C3%B3nica:%F0%A0%9C%8E%F0%A0%9C%B1@example.com">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 username => <<"mónica"/utf8>>,
                                 password => <<"𠜎𠜱"/utf8>>})),
   ?_assertEqual(<<"http://example.com/foo/bar%20baz">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 path => <<"/foo/bar baz">>})),
   ?_assertEqual(<<"http://example.com">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 query => []})),
   ?_assertEqual(<<"http://example.com/">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 path => <<"/">>,
                                 query => []})),
   ?_assertEqual(<<"http://example.com?foo=%C3%A9t%C3%A9&x%20y=&=hello">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 query => [{<<"foo">>, <<"été"/utf8>>},
                                           {<<"x y">>, <<>>},
                                           {<<>>, <<"hello">>}]})),
   ?_assertEqual(<<"http://example.com#foo">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 fragment => <<"foo">>})),
   ?_assertEqual(<<"http://example.com">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 fragment => <<"">>})),
   ?_assertEqual(<<"http://example.com?a=b#foo">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 query => [{<<"a">>, <<"b">>}],
                                 fragment => <<"foo">>})),
   ?_assertEqual(<<"http://example.com/#%C3%A9t%C3%A9">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 path => <<"/">>,
                                 fragment => <<"été"/utf8>>})),
   ?_assertEqual(<<"http://example.com#/foo%20bar?">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 fragment => <<"/foo bar?">>})),
   ?_assertEqual(<<"http://example.com?a%3Db=c%3Dd">>,
                 uri:serialize(#{scheme => <<"http">>,
                                 host => <<"example.com">>,
                                 query => [{<<"a=b">>, <<"c=d">>}]}))].

parse_without_authority_test_() ->
  [?_assertEqual({ok, #{scheme => <<"file">>}},
                 uri:parse(<<"file:">>)),
   ?_assertEqual({ok, #{scheme => <<"file+foo-1.1">>}},
                 uri:parse(<<"file+foo-1.1:">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<"/">>}},
                 uri:parse(<<"file:/">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<"/foo/bar">>}},
                 uri:parse(<<"file:/foo/bar">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<"foo/bar">>}},
                 uri:parse(<<"file:foo/bar">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<".">>}},
                 uri:parse(<<"file:.">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<"/foo">>,
                        query => [{<<"a">>, <<"1">>}]}},
                 uri:parse(<<"file:/foo?a=1">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<"/foo">>,
                        fragment => <<"bye">>}},
                 uri:parse(<<"file:/foo#bye">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        query => [{<<"a">>, <<"1">>}]}},
                 uri:parse(<<"file:?a=1">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        fragment => <<"bye">>}},
                 uri:parse(<<"file:#bye">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        query => [{<<"a">>, <<"b">>}],
                        fragment => <<"bye">>}},
                 uri:parse(<<"file:?a=b#bye">>))].

parse_without_userinfo_test_() ->
  [?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"example.com">>}},
                 uri:parse(<<"http://example.com">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"example.com">>,
                        port => 80}},
                 uri:parse(<<"http://example.com:80">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"example.com">>,
                        query => [{<<"a">>, <<"1">>}]}},
                 uri:parse(<<"http://example.com?a=1">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"example.com">>,
                        fragment => <<"bye">>}},
                 uri:parse(<<"http://example.com#bye">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"example.com">>,
                        query => [{<<"a">>, <<"b">>}],
                        fragment => <<"bye">>}},
                 uri:parse(<<"http://example.com?a=b#bye">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"example.com">>,
                        path => <<"/">>,
                        query => [{<<"a">>, <<"1">>}]}},
                 uri:parse(<<"http://example.com/?a=1">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"example.com">>,
                        path => <<"/">>,
                        fragment => <<"bye">>}},
                 uri:parse(<<"http://example.com/#bye">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"example.com">>,
                        path => <<"/a/b/c">>,
                        query => [{<<"a">>, <<"b">>}],
                        fragment => <<"bye">>}},
                 uri:parse(<<"http://example.com/a/b/c?a=b#bye">>))].

parse_with_userinfo_test_() ->
  [?_assertEqual({ok, #{scheme => <<"http">>,
                        username => <<"bob">>,
                        host => <<"example.com">>}},
                 uri:parse(<<"http://bob@example.com">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        username => <<"bob">>,
                        host => <<"example.com">>,
                        port => 80}},
                 uri:parse(<<"http://bob@example.com:80">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        username => <<"bob">>,
                        password => <<"foo">>,
                        host => <<"example.com">>}},
                 uri:parse(<<"http://bob:foo@example.com">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        username => <<"bob">>,
                        password => <<"">>,
                        host => <<"example.com">>}},
                 uri:parse(<<"http://bob:@example.com">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        username => <<"">>,
                        password => <<"foo">>,
                        host => <<"example.com">>}},
                 uri:parse(<<"http://:foo@example.com">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        username => <<"">>,
                        password => <<"">>,
                        host => <<"example.com">>}},
                 uri:parse(<<"http://:@example.com">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        username => <<"bob">>,
                        password => <<"foo:bar">>,
                        host => <<"example.com">>}},
                 uri:parse(<<"http://bob:foo:bar@example.com">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        username => <<"mónica"/utf8>>,
                        password => <<"𠜎𠜱"/utf8>>,
                        host => <<"example.com">>}},
                 uri:parse(<<"http://m%C3%B3nica:%F0%A0%9C%8E%F0%A0%9C%B1@example.com">>))].

parse_with_host_port_test_() ->
  [?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"10.0.150.3">>}},
                 uri:parse(<<"http://10.0.150.3">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"[::1]">>}},
                 uri:parse(<<"http://[::1]">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"[fc00:0001:0002:0003:0004:0005]">>}},
                 uri:parse(<<"http://[fc00:0001:0002:0003:0004:0005]">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"[v1.fc00::1%2542]">>}},
                 uri:parse(<<"http://[v1.fc00::1%2542]">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"foo.bar.example.com">>}},
                 uri:parse(<<"http://foo.bar.example.com">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"example.com.">>}},
                 uri:parse(<<"http://example.com.">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"net.">>}},
                 uri:parse(<<"http://net.">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<".">>}},
                 uri:parse(<<"http://.">>))].

parse_with_path_test_() ->
  [?_assertEqual({ok, #{scheme => <<"file">>,
                        host => <<"">>}},
                 uri:parse(<<"file://">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        host => <<"">>,
                        path => <<"/">>}},
                 uri:parse(<<"file:///">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        host => <<"">>,
                        path => <<"/a/b/c">>}},
                 uri:parse(<<"file:///a/b/c">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        host => <<"">>,
                        path => <<"/a/b/c/">>}},
                 uri:parse(<<"file:///a/b/c/">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        host => <<"">>,
                        path => <<"/./etc/passwd">>}},
                 uri:parse(<<"file:///./etc/passwd">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        host => <<"">>,
                        path => <<"/foo//bar///">>}},
                 uri:parse(<<"file:///foo//bar///">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<"foo">>}},
                 uri:parse(<<"file:foo">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<"foo/">>}},
                 uri:parse(<<"file:foo/">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<"/dir;v=1.0/file">>}},
                 uri:parse(<<"file:/dir;v=1.0/file">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<"/:/@">>}},
                 uri:parse(<<"file:/:/@">>)),
   ?_assertEqual({ok, #{scheme => <<"file">>,
                        path => <<":">>}},
                 uri:parse(<<"file::">>))].

parse_with_query_test_() ->
  [?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        query => []}},
                 uri:parse(<<"http://?">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        query => [{<<"a">>, <<"1">>}]}},
                 uri:parse(<<"http://?a=1">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        query => [{<<"foo">>, <<"/   /">>},
                                  {<<"bar">>, <<"?">>}]}},
                 uri:parse(<<"http://?foo=/+%20+/&bar=?">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        query => [{<<"a">>, <<"(b">>},
                                  {<<"c)@d">>, <<"">>}]}},
                 uri:parse(<<"http://?a=(b&c)@d">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        query => [{<<"foo">>, <<"">>},
                                  {<<"">>, <<"bar">>},
                                  {<<"">>, <<"">>}]}},
                 uri:parse(<<"http://?foo=&=bar&=">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        query => [{<<"=foo&">>, <<"&bar=baz&">>}]}},
                 uri:parse(<<"http://?%3Dfoo%26=%26bar%3Dbaz%26">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        query => [{<<"a">>, <<"b c d">>},
                                  {<<"été"/utf8>>, <<"à"/utf8>>}]}},
                 uri:parse(<<"http://?a=b%20c%20d&%C3%A9t%C3%A9=%C3%A0">>))].

parse_with_fragment_test_() ->
  [?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        fragment => <<"">>}},
                 uri:parse(<<"http://#">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        fragment => <<"foo">>}},
                 uri:parse(<<"http://#foo">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        fragment => <<"foo bar">>}},
                 uri:parse(<<"http://#foo%20bar">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        fragment => <<"foo/bar">>}},
                 uri:parse(<<"http://#foo/bar">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        fragment => <<"?a/b/c">>}},
                 uri:parse(<<"http://#?a/b/c">>)),
   ?_assertEqual({ok, #{scheme => <<"http">>,
                        host => <<"">>,
                        fragment => <<"#">>}},
                 uri:parse(<<"http://#%23">>))].

parse_with_relative_reference_test_() ->
  [?_assertEqual({ok, #{}},
                 uri:parse(<<"">>)),
   ?_assertEqual({ok, #{path => <<"foo">>}},
                 uri:parse(<<"foo">>)),
   ?_assertEqual({ok, #{path => <<"./">>}},
                 uri:parse(<<"./">>)),
   ?_assertEqual({ok, #{path => <<"/foo/bar">>}},
                 uri:parse(<<"/foo/bar">>)),
   ?_assertEqual({ok, #{host => <<"example.com">>}},
                 uri:parse(<<"//example.com">>)),
   ?_assertEqual({ok, #{username => <<"bob">>,
                        password => <<"foo">>,
                        host => <<"example.com">>}},
                 uri:parse(<<"//bob:foo@example.com">>)),
   ?_assertEqual({ok, #{path => <<"foo">>,
                        query => [{<<"a">>, <<"b">>}],
                        fragment => <<"bar">>}},
                 uri:parse(<<"foo?a=b#bar">>))].

percent_decode_test_() ->
  [?_assertEqual({ok, <<"">>},
                 uri:percent_decode(<<"">>)),
   ?_assertEqual({ok, <<"foo">>},
                 uri:percent_decode(<<"foo">>)),
   ?_assertEqual({ok, <<" ">>},
                 uri:percent_decode(<<"%20">>)),
   ?_assertEqual({ok, <<"abc">>},
                 uri:percent_decode(<<"%61%62%63">>)),
   ?_assertEqual({ok, <<"aà€𝄞"/utf8>>},
                 uri:percent_decode(<<"%61%c3%a0%e2%82%AC%f0%9d%84%9E">>)),
   ?_assertEqual({error, {truncated_percent_sequence, <<"%">>}},
                 uri:percent_decode(<<"%">>)),
   ?_assertEqual({error, {truncated_percent_sequence, <<"%6">>}},
                 uri:percent_decode(<<"%6">>)),
   ?_assertEqual({error, {invalid_hex_digit, $g}},
                 uri:percent_decode(<<"%6g">>)),
   ?_assertEqual({error, {invalid_hex_digit, $,}},
                 uri:percent_decode(<<"%,1">>))].

percent_encode_test_() ->
  IsValid = fun (C) -> (C >= $a andalso C =< $z)
                         orelse C =:= $/ orelse C =:= $? end,
  [?_assertEqual(<<"">>,
                 uri:percent_encode(<<"">>, IsValid)),
   ?_assertEqual(<<"abc">>,
                 uri:percent_encode(<<"abc">>, IsValid)),
   ?_assertEqual(<<"/a?b?%23?c/">>,
                 uri:percent_encode(<<"/a?b?#?c/">>, IsValid)),
   ?_assertEqual(<<"%C3%A9t%C3%A9">>,
                 uri:percent_encode(<<"été"/utf8>>, IsValid)),
   ?_assertEqual(<<"a%C3%A0%E2%82%AC%F0%9D%84%9E">>,
                 uri:percent_encode(<<"aà€𝄞"/utf8>>, IsValid))].

resolve_reference_test_() ->
  %% See RFC 3986 5.4.1 and 5.4.2.
  Resolve = fun (Ref, Base) ->
                {ok, RefURI} = uri:parse(Ref),
                {ok, BaseURI} = uri:parse(Base),
                URI = uri:resolve_reference(RefURI, BaseURI),
                uri:serialize(URI)
            end,
  [?_assertEqual(<<"g:h">>,
                Resolve(<<"g:h">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g">>,
                 Resolve(<<"g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g">>,
                 Resolve(<<"./g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g/">>,
                 Resolve(<<"g/">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/g">>,
                 Resolve(<<"/g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://g">>,
                 Resolve(<<"//g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/d;p?y=">>,
                 Resolve(<<"?y">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g?y=">>,
                 Resolve(<<"g?y">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/d;p?q=#s">>,
                 Resolve(<<"#s">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g#s">>,
                 Resolve(<<"g#s">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g?y=#s">>,
                 Resolve(<<"g?y#s">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/;x">>,
                 Resolve(<<";x">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g;x">>,
                 Resolve(<<"g;x">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g;x?y=#s">>,
                 Resolve(<<"g;x?y#s">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/d;p?q=">>,
                 Resolve(<<"">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/">>,
                 Resolve(<<".">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/">>,
                 Resolve(<<"./">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/">>,
                 Resolve(<<"..">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/">>,
                 Resolve(<<"../">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/g">>,
                 Resolve(<<"../g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/">>,
                 Resolve(<<"../..">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/">>,
                 Resolve(<<"../../">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/g">>,
                 Resolve(<<"../../g">>, <<"http://a/b/c/d;p?q">>)),

   ?_assertEqual(<<"http://a/g">>,
                 Resolve(<<"../../../g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/g">>,
                 Resolve(<<"../../../../g">>, <<"http://a/b/c/d;p?q">>)),

   ?_assertEqual(<<"http://a/g">>,
                 Resolve(<<"/./g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/g">>,
                 Resolve(<<"/../g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g.">>,
                 Resolve(<<"g.">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/.g">>,
                 Resolve(<<".g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g..">>,
                 Resolve(<<"g..">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/..g">>,
                 Resolve(<<"..g">>, <<"http://a/b/c/d;p?q">>)),

   ?_assertEqual(<<"http://a/b/g">>,
                 Resolve(<<"./../g">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g/">>,
                 Resolve(<<"./g/.">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g/h">>,
                 Resolve(<<"g/./h">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/h">>,
                 Resolve(<<"g/../h">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g;x=1/y">>,
                 Resolve(<<"g;x=1/./y">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/y">>,
                 Resolve(<<"g;x=1/../y">>, <<"http://a/b/c/d;p?q">>)),

   ?_assertEqual(<<"http://a/b/c/g?y%2F.%2Fx=">>,
                 Resolve(<<"g?y/./x">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g?y%2F..%2Fx=">>,
                 Resolve(<<"g?y/../x">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g#s/./x">>,
                 Resolve(<<"g#s/./x">>, <<"http://a/b/c/d;p?q">>)),
   ?_assertEqual(<<"http://a/b/c/g#s/../x">>,
                 Resolve(<<"g#s/../x">>, <<"http://a/b/c/d;p?q">>)),

   ?_assertEqual(<<"http:g">>,
                 Resolve(<<"http:g">>, <<"http://a/b/c/d;p?q">>))].
