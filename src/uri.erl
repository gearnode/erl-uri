%% Copyright (c) 2020-2022 Exograd SAS.
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

-module(uri).

-export([path/1, query/1,
         query_parameter/2, query_parameter/3,
         find_query_parameter/2, has_query_parameter/2,
         add_query_parameter/3, add_query_parameters/2,
         remove_query_parameter/2, remove_query_parameters/2,
         fragment/1,
         serialize/1, parse/1, parse_query/1,
         percent_encode/2, percent_decode/1, percent_decode/2,
         encode_path/1,
         encode_query/1,
         resolve_reference/2,
         format_error/1]).

-export_type([uri/0,
              scheme/0, username/0, password/0, host/0, port_number/0, path/0,
              query/0, fragment/0,
              error_reason/0,
              percent_decoding_options/0, percent_decoding_error_reason/0]).

-type uri() ::
        #{scheme => scheme(),
          username => username(),
          password => password(),
          host => host(),
          port => port_number(),
          path => path(),
          query => query(),
          fragment => fragment()}.

-type scheme() :: binary().
-type username() :: binary().
-type password() :: binary().
-type host() :: binary().
-type port_number() :: 0..65535.
-type path() :: binary().
-type query() :: [{binary(), binary()}].
-type fragment() :: binary().

-type parsing_state() ::
        scheme | authority | path | query | fragment.

-type error_reason() ::
        {invalid_data, binary(), parsing_state(), uri()}
      | {invalid_host, binary()}
      | {truncated_host, binary()}
      | {invalid_port, binary()}
      | percent_decoding_error_reason().

-type percent_decoding_options() ::
        #{decode_plus => boolean()}.

-type percent_decoding_error_reason() ::
        {truncated_percent_sequence, binary()}
      | {invalid_hex_digit, integer()}.

-spec path(uri()) -> path().
path(#{path := Path}) ->
  Path;
path(_) ->
  <<"/">>.

-spec query(uri()) -> query().
query(#{query := Query}) ->
  Query;
query(_) ->
  [].

-spec query_parameter(uri(), binary()) -> binary().
query_parameter(URI, Name) ->
  case lists:keyfind(Name, 1, query(URI)) of
    {_, Value} ->
      Value;
    false ->
      error({unknown_query_parameter, Name, URI})
  end.

-spec query_parameter(uri(), binary(), binary()) -> binary().
query_parameter(URI, Name, DefaultValue) ->
  case lists:keyfind(Name, 1, query(URI)) of
    {_, Value} ->
      Value;
    false ->
      DefaultValue
  end.

-spec find_query_parameter(uri(), binary()) -> {ok, binary()} | error.
find_query_parameter(URI, Name) ->
  case lists:keyfind(Name, 1, query(URI)) of
    {_, Value} ->
      {ok, Value};
    false ->
      error
  end.

-spec has_query_parameter(uri(), binary()) -> boolean().
has_query_parameter(URI, Name) ->
  lists:keymember(Name, 1, query(URI)).

-spec add_query_parameter(uri(), binary(), binary()) -> uri().
add_query_parameter(URI, Name, Value) ->
  URI#{query => [{Name, Value} | query(URI)]}.

-spec add_query_parameters(uri(), [{binary(), binary()}]) -> uri().
add_query_parameters(URI, Pairs) ->
  URI#{query => query(URI) ++ Pairs}.

-spec remove_query_parameter(uri(), binary()) -> uri().
remove_query_parameter(URI, Name) ->
  URI#{query => [{N, V} || {N, V} <- query(URI), N =/= Name]}.

-spec remove_query_parameters(uri(), [binary()]) -> uri().
remove_query_parameters(URI, Names) ->
  URI#{query => lists:filter(fun ({Name, _}) ->
                                 not lists:member(Name, Names)
                             end, query(URI))}.

-spec fragment(uri()) -> fragment().
fragment(#{fragment := Fragment}) ->
  Fragment;
fragment(_) ->
  <<"">>.

-spec serialize(uri()) -> binary().
serialize(URI) ->
  SchemePart = serialize_scheme_part(URI),
  Authority = serialize_authority(URI),
  Path = serialize_path(URI),
  QueryPart = serialize_query_part(URI),
  FragmentPart = serialize_fragment_part(URI),
  Data = [SchemePart, Authority, Path, QueryPart, FragmentPart],
  iolist_to_binary(Data).

-spec serialize_scheme_part(uri()) -> iodata().
serialize_scheme_part(#{scheme := Scheme}) ->
  [Scheme, $:];
serialize_scheme_part(_) ->
  [].

-spec serialize_authority(uri()) -> iodata().
serialize_authority(URI = #{host := Host}) ->
  UserInfo = serialize_userinfo_part(URI),
  HostPart = case binary:match(Host, <<":">>) of
               nomatch -> Host;
               _ -> [$[, Host, $]]
             end,
  PortPart = serialize_port_part(URI),
  [<<"//">>, UserInfo, HostPart, PortPart];
serialize_authority(_) ->
  [].

-spec serialize_path(uri()) -> iodata().
serialize_path(#{path := Path}) ->
  encode_path(Path);
serialize_path(_) ->
  [].

-spec serialize_userinfo_part(uri()) -> iodata().
serialize_userinfo_part(#{username := Username, password := Password}) ->
  [encode_username(Username), $:, encode_password(Password), $@];
serialize_userinfo_part(#{username := Username}) ->
  [encode_username(Username), $@];
serialize_userinfo_part(#{password := Password}) ->
  [$:, encode_password(Password), $@];
serialize_userinfo_part(_) ->
  [].

-spec serialize_port_part(uri()) -> iodata().
serialize_port_part(#{port := Port}) ->
  [$:, integer_to_binary(Port)];
serialize_port_part(_) ->
  [].

-spec serialize_query_part(uri()) -> iodata().
serialize_query_part(#{query := Query}) when length(Query) > 0 ->
  [$?, serialize_query(Query)];
serialize_query_part(_) ->
  [].

-spec serialize_fragment_part(uri()) -> iodata().
serialize_fragment_part(#{fragment := Fragment}) when byte_size(Fragment) > 0 ->
  [$#, encode_fragment(Fragment)];
serialize_fragment_part(_) ->
  [].

-spec serialize_query(query()) -> iodata().
serialize_query(Query) ->
  Parts = lists:map(fun ({K, V}) ->
                        [encode_query_value(K), $=, encode_query_value(V)]
                    end, Query),
  lists:join($&, Parts).

-spec encode_query(query()) -> binary().
encode_query(Query) when length(Query) > 0 ->
  iolist_to_binary(serialize_query(Query));
encode_query(_) ->
  <<>>.

-spec encode_path(path()) -> binary().
encode_path(Path) ->
  percent_encode(Path, fun is_valid_path_char/1).

-spec is_valid_path_char(byte()) -> boolean().
is_valid_path_char(C) when C =:= $/ ->
  true;
is_valid_path_char(C) ->
  is_pchar(C).

-spec encode_username(username()) -> binary().
encode_username(Username) ->
  percent_encode(Username, fun is_valid_username_char/1).

-spec is_valid_username_char(byte()) -> boolean().
is_valid_username_char(C) ->
  is_pchar(C).

-spec encode_password(password()) -> binary().
encode_password(Password) ->
  percent_encode(Password, fun is_valid_password_char/1).

-spec is_valid_password_char(byte()) -> boolean().
is_valid_password_char(C) when C =:= $: ->
  true;
is_valid_password_char(C) ->
  is_pchar(C).

-spec encode_query_value(binary()) -> binary().
encode_query_value(Value) ->
  percent_encode(Value, fun is_valid_query_char/1).

-spec is_valid_query_char(byte()) -> boolean().
is_valid_query_char(C) ->
  %% While the grammar in RFC 3986 defines valid query characters as pchars,
  %% '/' or '?', we actually want to know if the character can be represented
  %% without being encoded as an hexadecimal sequence. The only characters
  %% which can be included in a query value without being encoded are
  %% characters of the unreserved set.
  is_unreserved_char(C).

-spec encode_fragment(fragment()) -> binary().
encode_fragment(Fragment) ->
  percent_encode(Fragment, fun is_valid_fragment_char/1).

-spec is_valid_fragment_char(byte()) -> boolean().
is_valid_fragment_char(C) when C =:= $/; C =:= $? ->
  true;
is_valid_fragment_char(C) ->
  is_pchar(C).

-spec is_pchar(byte()) -> boolean().
is_pchar(C) when C =:= $:; C =:= $@ ->
  true;
is_pchar(C) ->
  is_unreserved_char(C) orelse is_sub_delim_char(C).

-spec is_unreserved_char(byte()) -> boolean().
is_unreserved_char(C) when C >= $a, C =< $z;
                           C >= $A, C =< $Z;
                           C >= $0, C =< $9;
                           C =:= $-; C =:= $.; C =:= $_; C =:= $~ ->
  true;
is_unreserved_char(_) ->
  false.

-spec is_sub_delim_char(byte()) -> boolean().
is_sub_delim_char(C) when C =:= $!; C =:= $$; C =:= $&; C =:= $'; C =:= $(;
                          C =:= $); C =:= $*; C =:= $+; C =:= $,; C =:= $;;
                          C =:= $= ->
  true;
is_sub_delim_char(_) ->
  false.

-spec parse_query(binary()) -> {ok, query()} | {error, error_reason()}.
parse_query(Bin) ->
  try
    {ok, do_parse_query(Bin)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse(binary()) -> {ok, uri()} | {error, error_reason()}.
parse(Data) ->
  try
    URI = parse(scheme, Data, #{}),
    {ok, URI}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse(parsing_state(), binary(), uri()) -> uri().

parse(scheme, Data = <<C, _/binary>>, URI) when C =/= $/ ->
  case binary:split(Data, <<":">>) of
    [Scheme, Rest] ->
      URI2 = URI#{scheme => Scheme},
      parse(authority, Rest, URI2);
    _ ->
      parse(path, Data, URI)
  end;
parse(scheme, Data, URI) ->
  parse(authority, Data, URI);

parse(authority, <<"//", Data/binary>>, URI) ->
  case split2(Data, [<<"/">>, <<"?">>, <<"#">>]) of
    [Authority, Rest] ->
      URI2 = parse_uri_authority(URI, Authority),
      parse(path, Rest, URI2);
    [Authority] ->
      parse_uri_authority(URI, Authority)
  end;
parse(authority, Data, URI) ->
  parse(path, Data, URI);

parse(path, Data = <<C, _/binary>>, URI) when C =/= $?, C =/= $# ->
  case split2(Data, [<<"?">>, <<"#">>]) of
    [Path, Rest] ->
      URI2 = URI#{path => do_percent_decode(Path)},
      parse(query, Rest, URI2);
    [Path] ->
      URI#{path => do_percent_decode(Path)}
  end;
parse(path, Data, URI) ->
  parse(query, Data, URI);

parse(query, <<$?, Data/binary>>, URI) ->
  case split2(Data, <<"#">>) of
    [QueryData, Rest] ->
      URI2 = URI#{query => do_parse_query(QueryData)},
      parse(fragment, Rest, URI2);
    [QueryData] ->
      URI#{query => do_parse_query(QueryData)}
  end;
parse(query, Data, URI) ->
  parse(fragment, Data, URI);

parse(fragment, <<$#, FragmentData/binary>>, URI) ->
  URI#{fragment => do_percent_decode(FragmentData)};
parse(fragment, <<>>, URI) ->
  URI;

parse(State, Data, URI) ->
  throw({error, {invalid_data, Data, State, URI}}).

-spec parse_uri_authority(uri(), binary()) -> uri().
parse_uri_authority(URI, Data) ->
  case binary:split(Data, <<"@">>) of
    [UserInfo, Rest] ->
      URI2 = case split2(UserInfo, <<":">>) of
               [U, <<":">>] ->
                 Username = do_percent_decode(U),
                 URI#{username => Username, password => <<>>};
               [U, <<":", P/binary>>] ->
                 Username = do_percent_decode(U),
                 Password = do_percent_decode(P),
                 URI#{username => Username, password => Password};
               [U] ->
                 Username = do_percent_decode(U),
                 URI#{username => Username}
             end,
      parse_uri_host_and_port(URI2, Rest);
    _ ->
      parse_uri_host_and_port(URI, Data)
  end.

-spec parse_uri_host_and_port(uri(), binary()) -> uri().
parse_uri_host_and_port(URI, <<$[, Data/binary>>) ->
  %% ":" can appear in the host part when it is an IPv6 address. When this is
  %% the case, the address is delimited by brackets.
  case binary:split(Data, <<"]">>) of
    [Host, <<>>] ->
      URI#{host => Host};
    [Host, <<$:, PortData/binary>>] ->
      URI#{host => Host, port => decode_port(PortData)};
    [_Host, _Rest] ->
      throw({error, {invalid_host, Data}});
    _ ->
      throw({error, {truncated_host, Data}})
  end;
parse_uri_host_and_port(URI, Data) ->
  case binary:split(Data, <<":">>) of
    [Host, PortData] ->
      URI#{host => Host, port => decode_port(PortData)};
    [Host] ->
      URI#{host => Host}
  end.

-spec decode_port(binary()) -> port_number().
decode_port(Data) ->
  try
    case binary_to_integer(Data) of
      Port when Port < 0; Port > 65535 ->
        throw({error, {invalid_port, Data}});
      Port ->
        Port
    end
  catch error:badarg ->
      throw({error, {invalid_port, Data}})
  end.

-spec do_parse_query(binary()) -> query().
do_parse_query(<<>>) ->
  [];
do_parse_query(Query) ->
  Parts = binary:split(Query, <<"&">>, [global]),
  Split = fun (Part) ->
              case binary:split(Part, <<"=">>) of
                [Key, Value] ->
                  {decode_query_value(Key), decode_query_value(Value)};
                [Key] ->
                  {decode_query_value(Key), <<>>}
              end
          end,
  lists:map(Split, Parts).

-spec decode_query_value(binary()) -> binary().
decode_query_value(Data) ->
  do_percent_decode(Data, #{decode_plus => true}).

-spec percent_encode(binary(), IsValidChar :: fun((byte()) -> boolean())) ->
        binary().
percent_encode(Data, IsValidChar) ->
  percent_encode(Data, IsValidChar, <<>>).

-spec percent_encode(binary(), fun((byte()) -> boolean()),
                     Acc :: binary()) -> binary().
percent_encode(<<>>, _, Acc) ->
  Acc;
percent_encode(<<C, Data/binary>>, IsValidChar, Acc) ->
  case IsValidChar(C) of
    true ->
      percent_encode(Data, IsValidChar, <<Acc/binary, C>>);
    false ->
      C1 = integer_to_hex_digit(C bsr 4),
      C2 = integer_to_hex_digit(C band 16#0f),
      percent_encode(Data, IsValidChar, <<Acc/binary, $%, C1, C2>>)
  end.

-spec percent_decode(binary()) ->
        {ok, binary()} | {error, percent_decoding_error_reason()}.
percent_decode(Data) ->
  percent_decode(Data, #{}).

-spec percent_decode(binary(), percent_decoding_options()) ->
        {ok, binary()} | {error, percent_decoding_error_reason()}.
percent_decode(Data, Options) ->
  try
    {ok, do_percent_decode(Data, Options)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec do_percent_decode(binary()) -> binary().
do_percent_decode(Data) ->
  do_percent_decode(Data, #{}).

-spec do_percent_decode(binary(), percent_decoding_options()) -> binary().
do_percent_decode(Data, Options) ->
  do_percent_decode(Data, Options, <<>>).

-spec do_percent_decode(binary(), percent_decoding_options(), Acc :: binary()) ->
        binary().
do_percent_decode(<<>>, _, Acc) ->
  Acc;
do_percent_decode(<<$%, C1, C2, Data/binary>>, Options, Acc) ->
  Hi = hex_digit_to_integer(C1),
  Lo = hex_digit_to_integer(C2),
  do_percent_decode(Data, Options, <<Acc/binary, Hi:4, Lo:4>>);
do_percent_decode(Data = <<$%, _/binary>>, _, _) when byte_size(Data) < 3 ->
  throw({error, {truncated_percent_sequence, Data}});
do_percent_decode(<<$+, Data/binary>>, Options = #{decode_plus := true}, Acc) ->
  do_percent_decode(Data, Options, <<Acc/binary, $\s>>);
do_percent_decode(<<C, Data/binary>>, Options, Acc) ->
  do_percent_decode(Data, Options, <<Acc/binary, C>>).

-spec integer_to_hex_digit(0..15) -> char().
integer_to_hex_digit(I) when I >= 0, I =< 9 ->
  $0 + I;
integer_to_hex_digit(I) when I < 16 ->
  $A + I - 10.

-spec hex_digit_to_integer(char()) -> 0..15.
hex_digit_to_integer(C) when C >= $0, C =< $9 ->
  C - $0;
hex_digit_to_integer(C) when C >= $a, C =< $f ->
  10 + C - $a;
hex_digit_to_integer(C) when C >= $A, C =< $F ->
  10 + C - $A;
hex_digit_to_integer(C) ->
  throw({error, {invalid_hex_digit, C}}).

-spec split2(Subject :: binary(),
             Pattern :: binary() | [binary()] | binary:cp()) ->
        [binary()].
split2(Subject, Pattern) ->
  case binary:match(Subject, Pattern) of
    {Start, _} ->
      [binary:part(Subject, {0, Start}),
       binary:part(Subject, {Start, byte_size(Subject)-Start})];
    nomatch ->
      [Subject]
  end.

-spec resolve_reference(Ref :: uri(), Base :: uri()) -> uri().
resolve_reference(Ref = #{scheme := _}, _Base) ->
  Ref#{path => remove_uri_dot_segments(Ref)};
resolve_reference(Ref = #{host := _}, #{scheme := Scheme}) ->
  Ref#{scheme => Scheme, path => remove_uri_dot_segments(Ref)};
resolve_reference(Ref, Base = #{scheme := _}) ->
  URI0 = case maps:find(fragment, Ref) of
           {ok, Fragment} ->
             Base#{fragment => Fragment};
           error ->
             Base
         end,
  case maps:get(path, Ref, <<>>) of
    <<>> ->
      case Ref of
        #{query := Query} ->
          URI0#{query => Query};
        _ ->
          URI0
      end;
    Path ->
      URI = case Ref of
              #{query := Query} ->
                URI0#{query => Query};
              _ ->
                maps:remove(query, URI0)
            end,
      case Path of
        <<$/, _/binary>> ->
          URI#{path => uri_paths:remove_dot_segments(Path)};
        _ ->
          BasePath = maps:get(path, Base, <<>>),
          BaseHasAuthority = maps:is_key(host, Base),
          Path2 = uri_paths:merge(BasePath, BaseHasAuthority, Path),
          URI#{path => uri_paths:remove_dot_segments(Path2)}
      end
  end;
resolve_reference(_Ref, Base) ->
  error({missing_base_uri_scheme, Base}).

-spec remove_uri_dot_segments(uri()) -> path().
remove_uri_dot_segments(URI) ->
  Path = maps:get(path, URI, <<>>),
  uri_paths:remove_dot_segments(Path).

-spec format_error(error_reason()) -> unicode:chardata().
format_error({invalid_data, Data, State, _}) ->
  io_lib:format("invalid data \"~ts\" in ~ts", [Data, State]);
format_error({invalid_host, Data}) ->
  io_lib:format("invalid host \"~ts\"", [Data]);
format_error({truncated_host, Data}) ->
  io_lib:format("truncated host \"~ts\"", [Data]);
format_error({invalid_port, Data}) ->
  io_lib:format("invalid port \"~ts\"", [Data]);
format_error({truncated_percent_sequence, Data}) ->
  io_lib:format("truncated percent-encoding sequence \"~ts\"", [Data]);
format_error({invalid_hex_digit, Digit}) ->
  io_lib:format("invalid hex digit '~ts'", [[Digit]]);
format_error(Term) ->
  io_lib:format("~0tp", [Term]).
