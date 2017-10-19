%% Copyright (c) 2017 Erik Reitsma <development@ernovation.com>
-module(fields_parser).

-export([parse_fields/2]).

parse_fields(Specs, Data) ->
    parse_fields(Specs, Data, #{}).

parse_fields([], Rest, Res) ->
    {Res, [], Rest};
parse_fields(Specs, <<>>, Res) ->
    {Res, Specs, <<>>};
parse_fields([{Name, Spec} | More], Data, Res) ->
    {Value, MoreData} = parse_field(Spec, Data),
    parse_fields(More, MoreData, Res#{Name => Value}).

parse_field(byte, <<B:8/little, More/binary>>) ->
    {B, More};
parse_field(word, <<W:16/little, More/binary>>) ->
    {W, More};
parse_field(dword, <<DW:32/little, More/binary>>) ->
    {DW, More};
parse_field(dword, <<QW:64/little, More/binary>>) ->
    {QW, More};
parse_field(_Other, Data) ->
    {{todo, Data}, <<>>}.
