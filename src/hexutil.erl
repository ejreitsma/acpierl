%% Copyright (c) 2017 Erik Reitsma <development@ernovation.com>
-module(hexutil).

-export([bin_to_hex/1]).

bin_to_hex(<<>>) ->
    " (0)";
bin_to_hex(Bin) ->
    tl(lists:append(
	 [[$  |byte_to_hex(Byte)] || <<Byte>> <= Bin])) 
	++ " (" ++ integer_to_list(size(Bin)) ++ ")".

byte_to_hex(Byte) ->
    tl(erlang:integer_to_list(256 + Byte, 16)).
