
%%%-------------------------------------------------------------------
%%% @author Novakov
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. lis 2014 20:39
%%%-------------------------------------------------------------------
-module(ds18b20).
-author("Novakov").

%% API
-export([read_temperature/1]).

read_temperature(DevicePath) ->
  {ok, F} = file:open(DevicePath,  [read]),

  {ok, Line} = file:read(F, 9 * 3),

  [A, B, C, D|_] = as_nums(Line, []),

  Temp = decode_temp(<<D:4, A:4, B:4>>),

  {ok, Temp}.

decode_temp(<<>>, _, Acc) -> Acc;
decode_temp(<<Bit:1, Bits/bitstring>>, BitValue, Acc) ->
  decode_temp(Bits, BitValue / 2.0, Acc + BitValue * Bit).

decode_temp(<<Sign:1, Bits:11/bitstring>>) ->
  decode_temp(Bits, 64, 0).

as_nums([], Acc) -> Acc;
as_nums([32 | Rest], Acc) -> as_nums(Rest, Acc);
as_nums([B1 | Rest], Acc) when B1 >= $a ->
  as_nums(Rest, Acc ++ [B1 - $a + 10]);
as_nums([B1 | Rest], Acc) ->
  as_nums(Rest, Acc ++ [B1 - $0]).
