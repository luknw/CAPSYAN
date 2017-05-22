-module(rPollution).

-behaviour(gen_server).

-export([
  start_link/0,
  stop/1,
  createMonitor/1,
  addStation/4,
  addValue/6,
  removeValue/5,
  getOneValue/5,
  getStationMean/4,
  getDailyMean/4,
  getMaximumVariationStation/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


createMonitor(Pid) -> gen_server:call(Pid, {createMonitor, []}).

addStation(Monitor, Name, Location, Pid) ->
  gen_server:call(Pid, {addStation, [Monitor, Name, Location]}).

addValue(Monitor, Station, Type, Date, Value, Pid) ->
  gen_server:call(Pid, {addValue, [Monitor, Station, Type, Date, Value]}).

removeValue(Monitor, Station, Type, Date, Pid) ->
  gen_server:call(Pid, {removeValue, [Monitor, Station, Type, Date]}).

getOneValue(Monitor, Station, Type, Date, Pid) ->
  gen_server:call(Pid, {getOneValue, [Monitor, Station, Type, Date]}).

getStationMean(Monitor, Station, Type, Pid) ->
  gen_server:call(Pid, {getStationMean, [Monitor, Station, Type]}).

getDailyMean(Monitor, Type, Day, Pid) ->
  gen_server:call(Pid, {getDailyMean, [Monitor, Type, Day]}).

getMaximumVariationStation(Monitor, Type, Pid) ->
  gen_server:call(Pid, {getMaximumVariationStation, [Monitor, Type]}).


start_link() -> gen_server:start_link(?MODULE, ok, []).

stop(Pid) -> gen_server:stop(Pid).


init(_State) -> {ok, _State}.

handle_call(Request, _From, State) -> {reply, apply(pollution, element(1, Request), element(2, Request)), State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(normal, _State) -> ok.
