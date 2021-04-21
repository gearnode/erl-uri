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

-module(uri_paths).

-export([join/1, merge/3,
         remove_dot_segments/1,
         remove_first_segment/1,
         remove_last_segment/1,
         remove_last_segment_and_slash/1]).

-spec join([binary()]) -> binary().
join([]) ->
  <<"/">>;
join(Parts) ->
  iolist_to_binary([[$/, uri:encode_path(Part)] || Part <- Parts]).

-spec merge(BasePath :: uri:path(), BaseHasAuthority :: boolean(),
            RefPath :: uri:path()) -> uri:path().
merge(<<>>, true, RefPath) ->
  <<$/, RefPath/binary>>;
merge(BasePath, _, RefPath) ->
  BasePath2 = remove_last_segment(BasePath),
  <<BasePath2/binary, RefPath/binary>>.

-spec remove_dot_segments(uri:path()) -> uri:path().
remove_dot_segments(Path) ->
  remove_dot_segments(Path, <<>>).

-spec remove_dot_segments(Path :: uri:path(), Acc :: uri:path()) -> uri:path().
remove_dot_segments(<<>>, Acc) ->
  Acc;
remove_dot_segments(<<"../", Path/binary>>, Acc) ->
  remove_dot_segments(Path, Acc);
remove_dot_segments(<<"./", Path/binary>>, Acc) ->
  remove_dot_segments(Path, Acc);
remove_dot_segments(<<"/./", Path/binary>>, Acc) ->
  remove_dot_segments(<<"/", Path/binary>>, Acc);
remove_dot_segments(<<"/.">>, Acc) ->
  remove_dot_segments(<<"/">>, Acc);
remove_dot_segments(<<"/../", Path/binary>>, Acc) ->
  remove_dot_segments(<<"/", Path/binary>>,
                      remove_last_segment_and_slash(Acc));
remove_dot_segments(<<"/..">>, Acc) ->
  remove_dot_segments(<<"/">>, remove_last_segment_and_slash(Acc));
remove_dot_segments(<<"..">>, Acc) ->
  remove_dot_segments(<<>>, Acc);
remove_dot_segments(<<".">>, Acc) ->
  remove_dot_segments(<<>>, Acc);
remove_dot_segments(Path, Acc) ->
  {Segment, Rest} = remove_first_segment(Path),
  remove_dot_segments(Rest, <<Acc/binary, Segment/binary>>).

-spec remove_first_segment(uri:path()) -> {Segment :: uri:path(), Rest :: uri:path()}.
remove_first_segment(<<$/, Path/binary>>) ->
  {Segment, Rest} = remove_first_segment(Path),
  {<<$/, Segment/binary>>, Rest};
remove_first_segment(Path) ->
  case binary:split(Path, <<"/">>) of
    [Segment, Rest] ->
      {Segment, <<$/, Rest/binary>>};
    _ ->
      {Path, <<>>}
  end.

-spec remove_last_segment(uri:path()) -> uri:path().
remove_last_segment(Path) ->
  case binary:match(Path, <<"/">>) of
    nomatch ->
      <<>>;
    _ ->
      Data = re:replace(Path, "/[^/]*$", <<"/">>),
      iolist_to_binary(Data)
  end.

-spec remove_last_segment_and_slash(uri:path()) -> uri:path().
remove_last_segment_and_slash(Path) ->
  case binary:match(Path, <<"/">>) of
    nomatch ->
      <<>>;
    _ ->
      Data = re:replace(Path, "/[^/]*$", <<"">>),
      iolist_to_binary(Data)
  end.
