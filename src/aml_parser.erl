%% Copyright (c) 2017 Erik Reitsma <development@ernovation.com>
-module(aml_parser).

-include("acpi.hrl").

-compile(export_all).

parse_file(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    parse_table(Bin).

parse_table(Bin) ->
    {Header, Data} = parse_header(Bin),
    put(indent, ""),
    {Header, parse_values(Data)}.
    
parse_header(<<Signature:4/binary,
	       Length:32/little,
	       Revision,
	       Checksum,
	       OEMID:6/binary,
	       OEMTableID:8/binary,
	       OEMRevision:32/little,
	       CreatorID:4/binary,
	       CreatorRevision:32/little,
	       Binary/binary>>) ->
    {
      #table_header{signature = Signature,
		    length = Length,
		    revision = Revision,
		    checksum = Checksum,
		    oem_id = OEMID,
		    oem_table_id = OEMTableID,
		    oem_revision = OEMRevision,
		    creator_id = CreatorID,
		    creator_revision = CreatorRevision},
      Binary}.

parse_values(Data) ->
    parse_values(Data, []).

parse_values(<<>>, Res) ->
    lists:reverse(Res);
parse_values(Data, Res) ->
    OldIndent = get(indent),
    put(indent, [$* | OldIndent]),
    io:format("~s Remaining data: ~s~n", [OldIndent, hexutil:bin_to_hex(Data)]),
    {More, Value} = parse_value(Data),
    io:format("~s Got ~p, left ~s~n", [OldIndent, Value, hexutil:bin_to_hex(More)]),
    put(indent, OldIndent),
    parse_values(More, [Value | Res]).

parse_value(<<16#00, Data/binary>>) ->
    {Data, zero};
parse_value(<<16#01, Data/binary>>) ->
    {Data, one};
parse_value(<<16#06, Data/binary>>) ->
    {Name1, M1} = parse_item(namestring, Data),
    {Name2, M2} = parse_item(namestring, M1),
    {M2, {alias, Name1, Name2}};
parse_value(<<16#08, Data/binary>>) ->
    {Name, M1} = parse_item(namestring, Data),
    io:format("~s Name: ~p~n", [get(indent), Name]),
    {M2, Object} = parse_value(M1),
    {M2, {name, Name, Object}};
parse_value(<<16#0a, Byte, Data/binary>>) ->
    {Data, {byte, Byte}};
parse_value(<<16#0b, Word:16/little, Data/binary>>) ->
    {Data, {word, Word}};
parse_value(<<16#0c, DWord:32/little, Data/binary>>) ->
    {Data, {dword, DWord}};
parse_value(<<16#0d, Data/binary>>) ->
    {String, More} = find_until(Data, 0),
    {More, {string, String}};
parse_value(<<16#0e, QWord:64/little, Data/binary>>) ->
    {Data, {qword, QWord}};
parse_value(<<16#10, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {Name, M2} = parse_item(namestring, D),
    {M1,
     {scope, Name, parse_values(M2)}};
parse_value(<<16#11, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {M2, BufferSize} = parse_value(D),
    {M1,
     {buffer, BufferSize, M2}};
parse_value(<<16#12, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    io:format("~s Package (size ~p) ", [get(indent), PkgLength]),
    LengthSize = size(Data) - size(More),
    RemainingLength = PkgLength - LengthSize,
    <<D:RemainingLength/binary, M1/binary>>  = More,
    <<NumElements, M2/binary>> = D,
    io:format("(~p)~n", [NumElements]),
    Values = parse_values(M2),
    {M1,
     {package, NumElements, Values}};
parse_value(<<16#14, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {Name, <<ArgCount, M2/binary>>} = parse_item(namestring, D),
    %io:format("Method: ~p~n", [Name]),
    {M1,
     {method, Name, ArgCount, parse_values(M2)}};
parse_value(<<C1, _/binary>> = D) when C1 >= 16#30,
				       C1 =< 16#39 ->
    {Name, More} = parse_item(namestring, D),
    {More, {name, Name}};
parse_value(<<C1, _/binary>> = D) when C1 >= 16#41,
				       C1 =< 16#5a ->
    {Name, More} = parse_item(namestring, D),
    {More, {name, Name}};
parse_value(<<16#5b, 16#01, Data/binary>>) ->
    {Name, M1} = parse_item(namestring, Data),
    <<SyncFlags, M2/binary>> = M1,
    {M2, {mutex, Name, SyncFlags}};
parse_value(<<16#5b, 16#02, Data/binary>>) ->
    {Name, M1} = parse_item(namestring, Data),
    {M1, {event, Name}};
parse_value(<<16#5b, 16#12, Data/binary>>) ->
    {Name1, M1} = parse_item(supername, Data),
    {Name2, M2} = parse_item(supername, M1), 
    {M2, {condrefof, Name1, Name2}};
parse_value(<<16#5b, 16#13, Data/binary>>) ->
    {M1, SourceBuff} = parse_value(Data),
    {M2, BitIndex} = parse_value(M1),
    {M3, NumBits} = parse_value(M2),
    {Name, M4} = parse_item(namestring, M3), 
    {M4, {createfield, SourceBuff, BitIndex, NumBits, Name}};
parse_value(<<16#5b, 16#20, Data/binary>>) ->
    {Value, M1} = parse_item(namestring, Data),
    {DDBHandleObject, M2} = parse_item(supername, M1),
    {M2, {load, Value, DDBHandleObject}};
parse_value(<<16#5b, 16#21, Data/binary>>) ->
    {M1, Value} = parse_value(Data),
    {M1, {stall, Value}};
parse_value(<<16#5b, 16#22, Data/binary>>) ->
    {M1, Value} = parse_value(Data),
    {M1, {sleep, Value}};
parse_value(<<16#5b, 16#30, Data/binary>>) ->
    {Data, revision};
parse_value(<<16#5b, 16#31, Data/binary>>) ->
    {Data, debug};
parse_value(<<16#5b, 16#32, Data/binary>>) ->
    <<FatalType, FatalCode:32/little, M1/binary>> = Data,
    {M2, FatalArg} = parse_value(M1),
    {M2, {fatal, FatalType, FatalCode, FatalArg}};
parse_value(<<16#5b, 16#33, Data/binary>>) ->
    {Data, timer};
parse_value(<<16#5b, 16#80, Data/binary>>) ->
    {Name, M1} = parse_item(namestring, Data),
    <<Space, M2/binary>> = M1,
    {M3, Offset} = parse_value(M2),
    {M4, Len} = parse_value(M3),
    {M4, {region, Name, Space, Offset, Len}};
parse_value(<<16#5b, 16#81, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {Name, <<Flags, M2/binary>>} = parse_item(namestring, D),
    {M1,
     {field, Name, Flags, parse_fields(M2)}};
parse_value(<<16#5b, 16#82, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {Name, M2} = parse_item(namestring, D),
    {M1, {device, Name, parse_values(M2)}};
parse_value(<<16#5b, 16#83, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {Name, M2} = parse_item(namestring, D),
    <<ProcID, PblkAddr:32/little, PblkLen, M3/binary>> = M2,
    {M1, {processor, Name, ProcID, PblkAddr, PblkLen, parse_values(M3)}};
parse_value(<<16#5b, 16#84, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {Name, M2} = parse_item(namestring, D),
    <<SystemLevel, ResourceLevel:16/little, M3/binary>> = M2,
    {M1, {powerres, Name, SystemLevel, ResourceLevel, parse_values(M3)}};
parse_value(<<16#5b, 16#85, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {Name, M2} = parse_item(namestring, D),
    {M1, {thermalzone, Name, parse_values(M2)}};
parse_value(<<16#5b, 16#86, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {Name1, M2} = parse_item(namestring, D),
    {Name2, M3} = parse_item(namestring, M2),
    <<Flags, M4/binary>> = M3,
    {M1,
     {indexfield, Name1, Name2, Flags, parse_fields(M4)}};
parse_value(<<16#5c, _/binary>> = D) ->
    {Name, More} = parse_item(namestring, D),
    {More, {name, Name}};
parse_value(<<16#5e, _/binary>> = D) ->
    {Name, More} = parse_item(namestring, D),
    {More, {name, Name}};
parse_value(<<16#5f, _/binary>> = D) ->
    {Name, More} = parse_item(namestring, D),
    {More, {name, Name}};
parse_value(<<16#60, Data/binary>>) ->
    {Data, local0};
parse_value(<<16#61, Data/binary>>) ->
    {Data, local1};
parse_value(<<16#62, Data/binary>>) ->
    {Data, local2};
parse_value(<<16#63, Data/binary>>) ->
    {Data, local3};
parse_value(<<16#64, Data/binary>>) ->
    {Data, local4};
parse_value(<<16#65, Data/binary>>) ->
    {Data, local5};
parse_value(<<16#66, Data/binary>>) ->
    {Data, local6};
parse_value(<<16#67, Data/binary>>) ->
    {Data, local7};
parse_value(<<16#68, Data/binary>>) ->
    {Data, arg0};
parse_value(<<16#69, Data/binary>>) ->
    {Data, arg1};
parse_value(<<16#6a, Data/binary>>) ->
    {Data, arg2};
parse_value(<<16#6b, Data/binary>>) ->
    {Data, arg3};
parse_value(<<16#6c, Data/binary>>) ->
    {Data, arg4};
parse_value(<<16#6d, Data/binary>>) ->
    {Data, arg5};
parse_value(<<16#6e, Data/binary>>) ->
    {Data, arg6};
parse_value(<<16#70, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {Name, M2} = parse_item(supername, M1),
    {M2, {store, Arg, Name}};
parse_value(<<16#71, Data/binary>>) ->
    {Name, M1} = parse_item(supername, Data),
    {M1, {refof, Name}};
parse_value(<<16#72, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'add', Arg1, Arg2, Target}};
parse_value(<<16#73, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'concat', Arg1, Arg2, Target}};
parse_value(<<16#74, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'subtract', Arg1, Arg2, Target}};
parse_value(<<16#75, Data/binary>>) ->
    {Name, M1} = parse_item(supername, Data),
    {M1, {increment, Name}};
parse_value(<<16#76, Data/binary>>) ->
    {Name, M1} = parse_item(supername, Data),
    {M1, {decrement, Name}};
parse_value(<<16#77, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'multiply', Arg1, Arg2, Target}};
parse_value(<<16#78, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target1} = parse_value(M2),
    {M4, Target2} = parse_value(M3),
    {M4, {'divide', Arg1, Arg2, Target1, Target2}};
parse_value(<<16#79, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'shiftleft', Arg1, Arg2, Target}};
parse_value(<<16#7a, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'shiftright', Arg1, Arg2, Target}};
parse_value(<<16#7b, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'and', Arg1, Arg2, Target}};
parse_value(<<16#7c, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {nand, Arg1, Arg2, Target}};
parse_value(<<16#7d, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'or', Arg1, Arg2, Target}};
parse_value(<<16#7e, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {nor, Arg1, Arg2, Target}};
parse_value(<<16#7f, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'xor', Arg1, Arg2, Target}};
parse_value(<<16#80, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M2, Target} = parse_value(M1),
    {M2, {'not', Arg, Target}};
parse_value(<<16#81, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M2, Target} = parse_value(M1),
    {M2, {'findsetleftbit', Arg, Target}};
parse_value(<<16#82, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M2, Target} = parse_value(M1),
    {M2, {'findsetrightbit', Arg, Target}};
parse_value(<<16#83, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M1, {derefof, Arg}};
parse_value(<<16#84, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'concatres', Arg1, Arg2, Target}};
parse_value(<<16#85, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {'mod', Arg1, Arg2, Target}};
parse_value(<<16#86, Data/binary>>) ->
    {Name, M1} = parse_item(supername, Data),
    {M2, Arg} = parse_value(M1),
    {M2, {notify, Name, Arg}};
parse_value(<<16#87, Data/binary>>) ->
    {Name, M1} = parse_item(supername, Data),
    {M1, {sizeof, Name}};
parse_value(<<16#88, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {M3, Target} = parse_value(M2),
    {M3, {index, Arg1, Arg2, Target}};
parse_value(<<16#89, Data/binary>>) ->
    {M1, SearchPkg} = parse_value(Data),
    <<MatchOp1, M2/binary>> = M1,
    {M3, Operand1} = parse_value(M2),
    <<MatchOp2, M4/binary>> = M3,
    {M5, Operand2} = parse_value(M4),
    {M6, StartIndex} = parse_value(M5),
    {M6, {match, SearchPkg, MatchOp1, Operand1, 
	  MatchOp2, Operand2, StartIndex}};
parse_value(<<16#8a, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {Name, M3} = parse_item(namestring, M2),
    {M3, {createdwordfield, Arg1, Arg2, Name}};
parse_value(<<16#8b, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {Name, M3} = parse_item(namestring, M2),
    {M3, {createwordfield, Arg1, Arg2, Name}};
parse_value(<<16#8c, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {Name, M3} = parse_item(namestring, M2),
    {M3, {createbytefield, Arg1, Arg2, Name}};
parse_value(<<16#8d, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {Name, M3} = parse_item(namestring, M2),
    {M3, {createbitfield, Arg1, Arg2, Name}};
parse_value(<<16#8f, Data/binary>>) ->
    {M1, Arg1} = parse_value(Data),
    {M2, Arg2} = parse_value(M1),
    {Name, M3} = parse_item(namestring, M2),
    {M3, {createqwordfield, Arg1, Arg2, Name}};
parse_value(<<16#90, Data/binary>>) ->
    {M1, V1} = parse_value(Data),
    {M2, V2} = parse_value(M1),
    {M2, {land, V1, V2}};
parse_value(<<16#91, Data/binary>>) ->
    {M1, V1} = parse_value(Data),
    {M2, V2} = parse_value(M1),
    {M2, {lor, V1, V2}};
parse_value(<<16#92, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M1, {lnot, Arg}};
parse_value(<<16#93, Data/binary>>) ->
    {M1, V1} = parse_value(Data),
    {M2, V2} = parse_value(M1),
    {M2, {lequal, V1, V2}};
parse_value(<<16#94, Data/binary>>) ->
    {M1, V1} = parse_value(Data),
    {M2, V2} = parse_value(M1),
    {M2, {lgreater, V1, V2}};
parse_value(<<16#95, Data/binary>>) ->
    {M1, V1} = parse_value(Data),
    {M2, V2} = parse_value(M1),
    {M2, {lless, V1, V2}};
parse_value(<<16#96, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M2, Target} = parse_value(M1),
    {M2, {'tobuffer', Arg, Target}};
parse_value(<<16#97, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M2, Target} = parse_value(M1),
    {M2, {'todecimalstring', Arg, Target}};
parse_value(<<16#98, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M2, Target} = parse_value(M1),
    {M2, {'tohexstring', Arg, Target}};
parse_value(<<16#99, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M2, Target} = parse_value(M1),
    {M2, {'tointeger', Arg, Target}};
parse_value(<<16#9f, Data/binary>>) ->
    {Data, continue};
parse_value(<<16#a0, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {M2, Condition} = parse_value(D),
    {M1,
     {'if', Condition, parse_values(M2)}};
parse_value(<<16#a1, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {M1,
     {elseop, parse_values(D)}};
parse_value(<<16#a2, Data/binary>>) ->
    {PkgLength, More} = parse_item(pkglength, Data),
    RemainingLength = PkgLength - (size(Data) - size(More)),
    <<D:RemainingLength/binary, M1/binary>> = More,
    {M2, Condition} = parse_value(D),
    {M1,
     {while, Condition, parse_values(M2)}};
parse_value(<<16#a3, Data/binary>>) ->
    {Data, noop};
parse_value(<<16#a4, Data/binary>>) ->
    {M1, Arg} = parse_value(Data),
    {M1, {return, Arg}};
parse_value(<<16#a5, Data/binary>>) ->
    {Data, break};
parse_value(<<16#cc, Data/binary>>) ->
    {Data, breakpoint};
parse_value(<<16#ff, Data/binary>>) ->
    {Data, ones};
parse_value(Other) ->
    io:format("UNSUPPORTED: ~p~n", [Other]),
    throw(x),
    {<<>>, 
     {unsupported, Other}}.

parse_item(nameseg, <<LeadNameChar, C1, C2, C3, More/binary>>) ->
    {<<LeadNameChar, C1, C2, C3>>,
     More};
%% DualNamePath
parse_item(namepath, <<16#2e, More/binary>>) ->
    {Name1, M1} = parse_item(nameseg, More),
    {Name2, M2} = parse_item(nameseg, M1),
    {[Name1, Name2], M2};
%% MultiNamePath
parse_item(namepath, <<16#2f, SegCount, More/binary>>) ->
    read_multiple(SegCount, nameseg, More);
%% NullName
parse_item(namepath, <<0, More/binary>>) ->
    {null, More};
parse_item(namepath, Data) ->
    parse_item(nameseg, Data);
parse_item(namestring, <<$\\, More/binary>>) ->
    {Name, M1} = parse_item(namepath, More),
    {{root, Name}, M1};
parse_item(namestring, <<$^, _/binary>> = Data) ->
    parse_item(prefixpath, Data);
parse_item(namestring, Data) ->
    parse_item(namepath, Data);
parse_item(supername, Data) ->
    %% should be simplename, debugobj, type6opcode 
    {More, Value} = parse_value(Data),
    {Value, More};
parse_item(prefixpath, <<$^, More/binary>>) ->
    {Prefix, M1} = parse_item(prefixpath, More),
    {[prefix | Prefix], M1};
parse_item(prefixpath, Data) ->
    {Name, More} = parse_item(namepath, Data),
    {[Name], More};
parse_item(pkglength, <<0:2, L:6, More/binary>>) ->
    {L, More};
parse_item(pkglength, <<C:2, 0:2, LSB:4, More/binary>>) ->
    L = C * 8,
    <<Val:L/little, M1/binary>> = More, 
    io:format("C = ~p, Val = ~p, LSB = ~p => length ~p~n", [C, Val, LSB, (Val bsl 4) + LSB]),
    {(Val bsl 4) + LSB, M1};
    %%{(LSB bsl L) + Val, M1};
parse_item(Type, Data) ->
    io:format("UNSUPPORTED ~p: ~p~n", [Type, Data]),
    {{Type, {unsupported, Data}}, <<>>}.

read_multiple(Count, Type, Data) ->
    read_multiple(Count, Type, Data, []).

read_multiple(0, _Type, Data, Res) ->
    {lists:reverse(Res), Data};
read_multiple(N, Type, Data, Res) ->
    {Value, More} = parse_item(Type, Data),
    read_multiple(N - 1, Type, More, [Value | Res]).

parse_fields(<<>>) ->
    [];
parse_fields(Data) ->
    {Value, More} = parse_field(Data),
    [Value | parse_fields(More)].

parse_field(<<16#03, AccessType, ExtendedAccessAttrib, AccessLength, More/binary>>) ->
    {{extended, AccessType, ExtendedAccessAttrib, AccessLength}, More};
parse_field(<<16#00, More/binary>>) ->
    {Length, M1} = parse_item(pkglength, More),
    {{reserved, Length}, M1};
parse_field(<<16#01, AccessType, AccessAttrib, More/binary>>) ->
    {{access, AccessType, AccessAttrib}, More};
parse_field(<<16#02, More/binary>>) ->
    {M1, Value} = parse_value(More),
    {{connect, Value}, M1};
parse_field(Data) ->
    {Name, More} = parse_item(nameseg, Data),
    {Length, M1} = parse_item(pkglength, More),
    {{Name, Length}, M1}.

find_until(Data, Value) ->
    find_until(Data, Value, 0).

find_until(Data, _Value, Index) when Index > size(Data) ->
    not_found;
find_until(Data, Value, Index) ->
    case binary:at(Data, Index) of
	Value ->
	    <<Res:Index/binary,
	      Value,
	      More/binary>> = Data,
	    {Res, More};
	_ ->
	    find_until(Data, Value, Index + 1)
    end.
