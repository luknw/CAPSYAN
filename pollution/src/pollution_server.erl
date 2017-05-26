-module(pollution_server).

-export([
  start/0,
  stop/0,
  createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getDailyMean/3,
  getMaximumVariationStation/2
]).


start() ->
  register(pollution_server, spawn(fun() -> serverLoop() end)).

stop() ->
  pollution_server ! {stop, self()},
  receive
    _Ignored -> _Ignored
  end.

createMonitor() ->
  request(createMonitor).

addStation(Monitor, Name, Location) ->
  request(addStation, [Monitor, Name, Location]).

addValue(Monitor, Station, Type, Date, Value) ->
  request(addValue, [Monitor, Station, Type, Date, Value]).

removeValue(Monitor, Station, Type, Date) ->
  request(removeValue, [Monitor, Station, Type, Date]).

getOneValue(Monitor, Station, Type, Date) ->
  request(getOneValue, [Monitor, Station, Type, Date]).

getStationMean(Monitor, Station, Type) ->
  request(getStationMean, [Monitor, Station, Type]).

getDailyMean(Monitor, Type, Day) ->
  request(getDailyMean, [Monitor, Type, Day]).

getMaximumVariationStation(Monitor, Type) ->
  request(getMaximumVariationStation, [Monitor, Type]).


% private

serverLoop() ->
  receive
    {request, Fun, Args, Pid} -> Pid ! {ok, apply(pollution, Fun, Args)}, serverLoop();
    {stop, Pid} -> Pid ! ok;
    _ -> serverLoop()
  end.


request(Fun) ->
  request(Fun, []).

request(Fun, Args) ->
  pollution_server ! {request, Fun, Args, self()},
  receive
    {ok, Result} -> Result;
    Error -> Error
  end.
