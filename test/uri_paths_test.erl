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

-module(uri_paths_test).

-include_lib("eunit/include/eunit.hrl").

join_test_() ->
  Join = fun uri_paths:join/1,
  [?_assertEqual(<<"/">>,
                 Join([])),
   ?_assertEqual(<<"/foo">>,
                 Join([<<"foo">>])),
   ?_assertEqual(<<"/foo/bar">>,
                 Join([<<"foo">>, <<"bar">>])),
   ?_assertEqual(<<"/%C3%A9t%C3%A9/%E2%9C%93/a%20b%20c">>,
                 Join([<<"été"/utf8>>, <<"✓"/utf8>>, <<"a b c">>])),
   ?_assertEqual(<<"/foo/bar">>,
                 Join([<<"/foo">>, <<"bar">>])),
   ?_assertEqual(<<"/foo/bar">>,
                 Join([<<"foo">>, <<"/bar">>])),
   ?_assertEqual(<<"/foo/bar">>,
                 Join([<<"/foo">>, <<"/bar">>]))].

remove_first_segment_test_() ->
  [?_assertEqual({<<"">>, <<"">>},
                 uri_paths:remove_first_segment(<<"">>)),
   ?_assertEqual({<<"foo">>, <<"">>},
                 uri_paths:remove_first_segment(<<"foo">>)),
   ?_assertEqual({<<"foo">>, <<"/">>},
                 uri_paths:remove_first_segment(<<"foo/">>)),
   ?_assertEqual({<<"/foo">>, <<"">>},
                 uri_paths:remove_first_segment(<<"/foo">>)),
   ?_assertEqual({<<"a">>, <<"/b/c">>},
                 uri_paths:remove_first_segment(<<"a/b/c">>)),
   ?_assertEqual({<<"/a">>, <<"/b/c">>},
                 uri_paths:remove_first_segment(<<"/a/b/c">>)),
   ?_assertEqual({<<"/a">>, <<"/b/c/">>},
                 uri_paths:remove_first_segment(<<"/a/b/c/">>))].

remove_last_segment_test_() ->
  [?_assertEqual(<<"">>,
                 uri_paths:remove_last_segment(<<"">>)),
   ?_assertEqual(<<"">>,
                 uri_paths:remove_last_segment(<<"foo">>)),
   ?_assertEqual(<<"foo/">>,
                 uri_paths:remove_last_segment(<<"foo/">>)),
   ?_assertEqual(<<"/">>,
                 uri_paths:remove_last_segment(<<"/foo">>)),
   ?_assertEqual(<<"a/b/">>,
                 uri_paths:remove_last_segment(<<"a/b/c">>)),
   ?_assertEqual(<<"/a/b/">>,
                 uri_paths:remove_last_segment(<<"/a/b/c">>)),
   ?_assertEqual(<<"/a/b/c/">>,
                 uri_paths:remove_last_segment(<<"/a/b/c/">>))].

remove_last_segment_and_slash_test_() ->
  [?_assertEqual(<<"">>,
                 uri_paths:remove_last_segment_and_slash(<<"">>)),
   ?_assertEqual(<<"">>,
                 uri_paths:remove_last_segment_and_slash(<<"foo">>)),
   ?_assertEqual(<<"foo">>,
                 uri_paths:remove_last_segment_and_slash(<<"foo/">>)),
   ?_assertEqual(<<"">>,
                 uri_paths:remove_last_segment_and_slash(<<"/foo">>)),
   ?_assertEqual(<<"a/b">>,
                 uri_paths:remove_last_segment_and_slash(<<"a/b/c">>)),
   ?_assertEqual(<<"/a/b">>,
                 uri_paths:remove_last_segment_and_slash(<<"/a/b/c">>)),
   ?_assertEqual(<<"/a/b/c">>,
                 uri_paths:remove_last_segment_and_slash(<<"/a/b/c/">>))].
