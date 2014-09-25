%%%-------------------------------------------------------------------
%%% @author Novakov
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. wrz 2014 11:05
%%%-------------------------------------------------------------------
-module(home_automation).
-author("Novakov").

%% API
-export([start/0]).

start() ->
  application:start(crypto),
  application:start(emysql),
  application:start(home_automation).
