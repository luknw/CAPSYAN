-module(rPollution).

-behaviour(gen_server).

-export([
  crash/0,
  start_link/0,
  stop/0,
  createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getDailyMean/3,
  getMaximumVariationStation/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


createMonitor() -> gen_server:call({global, rPollution}, {createMonitor, []}).

addStation(Monitor, Name, Location) ->
  gen_server:call({global, rPollution}, {addStation, [Monitor, Name, Location]}).

addValue(Monitor, Station, Type, Date, Value) ->
  gen_server:call({global, rPollution}, {addValue, [Monitor, Station, Type, Date, Value]}).

removeValue(Monitor, Station, Type, Date) ->
  gen_server:call({global, rPollution}, {removeValue, [Monitor, Station, Type, Date]}).

getOneValue(Monitor, Station, Type, Date) ->
  gen_server:call({global, rPollution}, {getOneValue, [Monitor, Station, Type, Date]}).

getStationMean(Monitor, Station, Type) ->
  gen_server:call({global, rPollution}, {getStationMean, [Monitor, Station, Type]}).

getDailyMean(Monitor, Type, Day) ->
  gen_server:call({global, rPollution}, {getDailyMean, [Monitor, Type, Day]}).

getMaximumVariationStation(Monitor, Type) ->
  gen_server:call({global, rPollution}, {getMaximumVariationStation, [Monitor, Type]}).


start_link() -> gen_server:start_link({global, rPollution}, ?MODULE, ok, []).

stop() -> gen_server:stop({global, rPollution}).


init(State) -> {ok, State}.

handle_call(Request, _From, State) -> {reply, apply(pollution, element(1, Request), element(2, Request)), State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(normal, _State) -> ok.


crash() ->
  gen_server:call({global, rPollution}, {crash, []}).
