-module(pollution_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  pollution_sup:start_link().
%%  {ok, SupervisorPid} = pollution_sup:start_link(),
%%  [{rPollution, _, _, _}] = supervisor:which_children(SupervisorPid),
%%  io:format("~p~n", [rPollution:createMonitor()]).

stop(_State) ->
  ok.
