%%%-------------------------------------------------------------------
%% @doc ejsonlog public API
%% @end
%%%-------------------------------------------------------------------

-module(ejsonlog_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ejsonlog_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
