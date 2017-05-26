-module(pollution).
-export([
  crash/0,
  createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getDailyMean/3,
  getMaximumVariationStation/2
]).

-include_lib("monitor.hrl").


createMonitor() ->
  #monitor{
    valuesByStation = #{}
    , stationsByName = #{}
    , stationsByLocation = #{}
    , stationNames = sets:new()
    , stationLocations = sets:new()
  }
.


addStation(Monitor = #monitor{}, Name, Location = {_Latitude, _Longitude}) ->
  #monitor{
    valuesByStation = Values
    , stationsByName = StationsByName
    , stationsByLocation = StationsByLocation
    , stationNames = Names
    , stationLocations = Locations
  } = Monitor,

  case {sets:is_element(Name, Names), sets:is_element(Location, Locations)} of
    {true, _} -> error(badarg, Name);
    {_, true} -> error(badarg, Location);
    _ ->
      Station = #station{name = Name, location = Location}
      , Monitor#monitor{
        valuesByStation = Values#{#station{name = Name, location = Location} => #{}}
        , stationsByName = StationsByName#{Name => Station}
        , stationsByLocation = StationsByLocation#{Location => Station}
        , stationNames = sets:add_element(Name, Names)
        , stationLocations = sets:add_element(Location, Locations)
      }
  end
.


addValue(Monitor = #monitor{}, Station = #station{}, Type, Date, Value) ->
  #monitor{valuesByStation = StationValues = #{Station := TypeDateValues}} = Monitor
  , case maps:find(Type, TypeDateValues) of
      {ok, DateValues} -> UpdatedValues = DateValues#{Date => Value}
      ; error -> UpdatedValues = #{Date => Value}
    end
  , Monitor#monitor{valuesByStation = StationValues#{Station => TypeDateValues#{Type => UpdatedValues}}}
;
addValue(Monitor = #monitor{}, StationLocation = {_Latitude, _Longitude}, Type, Date, Value) ->
  #monitor{stationsByLocation = #{StationLocation := Station}} = Monitor
  , addValue(Monitor, Station, Type, Date, Value)
;
addValue(Monitor = #monitor{}, StationName, Type, Date, Value) ->
  #monitor{stationsByName = #{StationName := Station}} = Monitor
  , addValue(Monitor, Station, Type, Date, Value)
.


removeValue(Monitor = #monitor{}, Station = #station{}, Type, Date) ->
  try
    #monitor{valuesByStation = StationValues = #{Station := TypeDateValues = #{Type := DateValues}}} = Monitor
    , Monitor#monitor{valuesByStation = StationValues#{Station => TypeDateValues#{Type => maps:remove(Date, DateValues)}}}
  catch
    error:{badmatch, _} -> Monitor
  end
;
removeValue(Monitor = #monitor{}, StationLocation = {_Latitude, _Longitude}, Type, Date) ->
  try
    #monitor{stationsByLocation = #{StationLocation := Station}} = Monitor
    , removeValue(Monitor, Station, Type, Date)
  catch
    error:{badmatch, _} -> Monitor
  end
;
removeValue(Monitor = #monitor{}, StationName, Type, Date) ->
  try
    #monitor{stationsByName = #{StationName := Station}} = Monitor
    , removeValue(Monitor, Station, Type, Date)
  catch
    error:{badmatch, _} -> Monitor
  end
.


getOneValue(Monitor = #monitor{}, Station = #station{}, Type, Date) ->
  #monitor{valuesByStation = #{Station := #{Type := #{Date := Value}}}} = Monitor
  , Value
;
getOneValue(Monitor = #monitor{}, StationLocation = {_Latitude, _Longitude}, Type, Date) ->
  #monitor{stationsByLocation = #{StationLocation := Station}} = Monitor
  , getOneValue(Monitor, Station, Type, Date)
; getOneValue(Monitor = #monitor{}, StationName, Type, Date) ->
  #monitor{stationsByName = #{StationName := Station}} = Monitor
  , getOneValue(Monitor, Station, Type, Date)
.


getStationMean(Monitor = #monitor{}, Station = #station{}, Type) ->
  #monitor{valuesByStation = #{Station := #{Type := DateValues}}} = Monitor
  , mean(maps:values(DateValues))
;
getStationMean(Monitor = #monitor{}, StationLocation = {_Latitude, _Longitude}, Type) ->
  #monitor{stationsByLocation = #{StationLocation := Station}} = Monitor
  , getStationMean(Monitor, Station, Type)
; getStationMean(Monitor = #monitor{}, StationName, Type) ->
  #monitor{stationsByName = #{StationName := Station}} = Monitor
  , getStationMean(Monitor, Station, Type)
.


getDailyMean(Monitor = #monitor{}, Type, Day) ->
  mean(
    lists:map(fun({_, Value}) -> Value end
      , lists:filter(fun({{DayKey, _}, _}) -> DayKey == Day end
        , lists:flatmap(fun({_, DateValues}) -> maps:to_list(DateValues) end
          , lists:filter(fun({TypeKey, _}) -> TypeKey == Type end
            , lists:flatmap(fun maps:to_list/1
              , maps:values(Monitor#monitor.valuesByStation)))))))
.


getMaximumVariationStation(Monitor = #monitor{}, Type) ->
  lists:max(
    lists:map(
      fun(Station) ->
        #{Station := TypeDateValues} = Monitor#monitor.valuesByStation
        , case maps:is_key(Type, TypeDateValues) of
            true -> {getStationVariation(Monitor, Station, Type), Station}
            ; false -> {}
          end
      end
      , maps:keys(Monitor#monitor.valuesByStation)))
.


% private
mean([]) -> error(badarg, []);
mean(List) -> mean(List, 0, 0).

mean([], Sum, Length) -> Sum / Length;
mean([Head | Tail], Sum, Length) -> mean(Tail, Sum + Head, Length + 1).


minmax([Head | Tail]) -> minmax(Tail, {Head, Head}).

minmax([], {Min, Max}) -> {Min, Max};
minmax([Head | Tail], {Min, Max}) -> minmax(Tail, {min(Head, Min), max(Head, Max)}).


getStationVariation(Monitor = #monitor{}, Station = #station{}, Type) ->
  {Min, Max} = minmax(maps:values(maps:get(Type, maps:get(Station, Monitor#monitor.valuesByStation))))
  , Max - Min
.


crash() ->
  1 / 0.