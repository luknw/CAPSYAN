-module(pollution_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/monitor.hrl").


createMonitor_returns_monitor_record_test() ->
  ?assertMatch(#monitor{}, pollution:createMonitor()).


createMonitor_returns_consistent_values_test() ->
  FirstReturned = pollution:createMonitor(),
  SecondReturned = pollution:createMonitor(),
  ?assertMatch(#monitor{}, FirstReturned),
  ?assertEqual(FirstReturned, SecondReturned).


addStation_to_empty_monitor_test() ->
  M0 = pollution:createMonitor(),
  Name = "name",
  Location = {12.34, 56.78},
  Station = #station{name = Name, location = Location},

  M1 = pollution:addStation(M0, Station#station.name, Station#station.location),
  ?assert(monitor_has_station(M1, Name, Location)).


addStation_wont_add_station_with_repeated_name_test() ->
  M0 = pollution:createMonitor(),
  Name = "name",
  Location = {12.34, 56.78},
  OtherLocation = {98.76, 54.32},

  M1 = pollution:addStation(M0, Name, Location),

  ?assertError(badarg, pollution:addStation(M1, Name, OtherLocation)).


addStation_wont_add_station_with_repeated_location_test() ->
  M0 = pollution:createMonitor(),
  Name = "name",
  OtherName = "other_name",
  Location = {12.34, 56.78},

  M1 = pollution:addStation(M0, Name, Location),

  ?assertError(badarg, pollution:addStation(M1, OtherName, Location)).


addStation_adds_a_few_stations_test() ->
  M0 = pollution:createMonitor(),

  M1 = pollution:addStation(M0, "0", {0, 0}),
  M2 = pollution:addStation(M1, "1", {1, 1}),
  M3 = pollution:addStation(M2, "2", {2, 2}),
  M4 = pollution:addStation(M3, "3", {3, 3}),

  ?assert(monitor_has_station(M4, "0", {0, 0})),
  ?assert(monitor_has_station(M4, "1", {1, 1})),
  ?assert(monitor_has_station(M4, "2", {2, 2})),
  ?assert(monitor_has_station(M4, "3", {3, 3})).


monitor_has_station(Monitor = #monitor{}, Name, Location) ->
  Station = #station{name = Name, location = Location},

  ?assertMatch(
    #monitor{valuesByStation = #{Station := #{}},
      stationsByName = #{Name := Station},
      stationsByLocation = #{Location := Station},
      stationNames = _,
      stationLocations = _},
    Monitor),
  ?assert(sets:is_element(Name, Monitor#monitor.stationNames)),
  ?assert(sets:is_element(Location, Monitor#monitor.stationLocations)),

  true.


addValue_add_one_value_test() ->
  Name = "0",
  Location = {0, 0},
  Station = #station{name = Name, location = Location},
  Type = "PM10",
  Date = {{2017, 01, 02}, {03, 04, 05}},
  Value = 666,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),

  M2 = pollution:addValue(M1, Name, Type, Date, Value),

  ?assertMatch(
    #{Station := #{Type := #{Date := Value}}},
    M2#monitor.valuesByStation).


addValue_overrides_value_with_same_station_type_and_date_test() ->
  Name = "0",
  Location = {0, 0},
  Station = #station{name = Name, location = Location},
  Type = "PM10",
  Date = {{2017, 01, 02}, {03, 04, 05}},
  Value = 666,
  OtherValue = 101,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, Date, Value),

  M3 = pollution:addValue(M2, Name, Type, Date, OtherValue),

  ?assertMatch(
    #{Station := #{Type := #{Date := OtherValue}}},
    M3#monitor.valuesByStation).


removeValue_from_station_test() ->
  Name = "0",
  Location = {0, 0},
  Station = #station{name = Name, location = Location},
  Type = "PM10",
  Date = {{2017, 01, 02}, {03, 04, 05}},
  Value = 666,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, Date, Value),

  M3 = pollution:removeValue(M2, Name, Type, Date),

  ?assertNotMatch(
    #{Station := #{Type := #{Date := Value}}},
    M3#monitor.valuesByStation).


removeValue_wont_remove_if_not_exists_test() ->
  Name = "0",
  Location = {0, 0},
  Type = "PM10",
  Date = {{2017, 01, 02}, {03, 04, 05}},

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),

  M2 = pollution:removeValue(M0, Name, Type, Date),
  M3 = pollution:removeValue(M1, Name, Type, Date),

  ?assertMatch(M0, M2),
  ?assertMatch(M1, M3).


getOneValue_from_station_test() ->
  Name = "0",
  Location = {0, 0},
  Type = "PM10",
  Date = {{2017, 01, 02}, {03, 04, 05}},
  Value = 666,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, Date, Value),

  ReturnedValue = pollution:getOneValue(M2, Name, Type, Date),

  ?assertEqual(Value, ReturnedValue).

getOneValue_error_if_type_not_exists_test() ->
  Name = "0",
  Location = {0, 0},
  Type = "PM10",
  OtherType = "other",
  Date = {{2017, 01, 02}, {03, 04, 05}},
  Value = 666,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, Date, Value),

  ?assertError({badmatch, _}, pollution:getOneValue(M2, Name, OtherType, Date)).


getOneValue_error_if_date_not_exists_test() ->
  Name = "0",
  Location = {0, 0},
  Type = "PM10",
  Date = {{2017, 01, 02}, {03, 04, 05}},
  OtherDate = {{1111, 11, 11}, {22, 22, 22}},
  Value = 666,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, Date, Value),

  ?assertError({badmatch, _}, pollution:getOneValue(M2, Name, Type, OtherDate)).


getStationMean_error_from_empty_station_test() ->
  Name = "0",
  Location = {0, 0},
  Type = "PM10",

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),

  ?assertError({badmatch, _}, pollution:getStationMean(M1, Name, Type)).


getStationMean_error_if_station_not_exists_test() ->
  Name = "0",
  OtherName = "1",
  Location = {0, 0},
  Type = "PM10",

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),

  ?assertError({badmatch, _}, pollution:getStationMean(M1, OtherName, Type)).


getStationMean_from_station_with_one_value_test() ->
  Name = "0",
  Location = {0, 0},
  Type = "PM10",
  Date = {{2017, 01, 02}, {03, 04, 05}},
  Value = 666.0,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, Date, Value),

  ?assertEqual(Value, pollution:getStationMean(M2, Name, Type)).


getStationMean_from_station_with_symmetric_values_test() ->
  Name = "0",
  Location = {0, 0},
  Type = "PM10",
  Middle = 100.0,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, {{2001, 01, 01}, {01, 01, 01}}, Middle - 3),
  M3 = pollution:addValue(M2, Name, Type, {{2002, 02, 02}, {02, 02, 02}}, Middle - 2),
  M4 = pollution:addValue(M3, Name, Type, {{2003, 03, 03}, {03, 03, 03}}, Middle - 1),
  M5 = pollution:addValue(M4, Name, Type, {{2004, 04, 04}, {04, 04, 04}}, Middle),
  M6 = pollution:addValue(M5, Name, Type, {{2005, 05, 05}, {05, 05, 05}}, Middle + 1),
  M7 = pollution:addValue(M6, Name, Type, {{2006, 06, 06}, {06, 06, 06}}, Middle + 2),
  M8 = pollution:addValue(M7, Name, Type, {{2007, 07, 07}, {07, 07, 07}}, Middle + 3),

  ?assertEqual(Middle, pollution:getStationMean(M8, Name, Type)).


getDailyMean_for_symmetric_values_on_multiple_stations_test() ->
  Name = "0", NameOne = "1", NameTwo = "2",
  Location = {0, 0}, LocationOne = {1, 1}, LocationTwo = {2, 2},
  Day = {2001, 01, 01},
  Type = "PM10",
  Middle = 100.0,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, {Day, {2, 2, 2}}, Middle - 2),
  M3 = pollution:addValue(M2, Name, Type, {Day, {3, 3, 3}}, Middle - 1),
  M4 = pollution:addValue(M3, Name, Type, {Day, {4, 4, 4}}, Middle),
  M5 = pollution:addValue(M4, Name, Type, {Day, {5, 5, 5}}, Middle + 1),
  M6 = pollution:addValue(M5, Name, Type, {Day, {6, 6, 6}}, Middle + 2),

  M7 = pollution:addStation(M6, NameOne, LocationOne),
  M8 = pollution:addValue(M7, NameOne, Type, {Day, {7, 7, 7}}, Middle - 2),
  M9 = pollution:addValue(M8, NameOne, Type, {Day, {8, 8, 8}}, Middle - 1),
  M10 = pollution:addValue(M9, NameOne, Type, {Day, {9, 9, 9}}, Middle),
  M11 = pollution:addValue(M10, NameOne, Type, {Day, {10, 10, 10}}, Middle + 1),
  M12 = pollution:addValue(M11, NameOne, Type, {Day, {11, 11, 11}}, Middle + 2),

  M13 = pollution:addStation(M12, NameTwo, LocationTwo),
  M14 = pollution:addValue(M13, NameTwo, Type, {Day, {12, 12, 12}}, Middle - 2),
  M15 = pollution:addValue(M14, NameTwo, Type, {Day, {13, 13, 13}}, Middle - 1),
  M16 = pollution:addValue(M15, NameTwo, Type, {Day, {14, 14, 14}}, Middle),
  M17 = pollution:addValue(M16, NameTwo, Type, {Day, {15, 15, 15}}, Middle + 1),
  M18 = pollution:addValue(M17, NameTwo, Type, {Day, {16, 16, 16}}, Middle + 2),

  ?assertEqual(Middle, pollution:getDailyMean(M18, Type, Day)).


getDailyMean_error_if_type_not_exists_test() ->
  Name = "0", NameOne = "1",
  Location = {0, 0}, LocationOne = {1, 1},
  Day = {2001, 01, 01},
  Type = "PM10",
  OtherType = "other",
  Middle = 100.0,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, {Day, {2, 2, 2}}, Middle - 2),
  M3 = pollution:addValue(M2, Name, Type, {Day, {3, 3, 3}}, Middle - 1),
  M4 = pollution:addValue(M3, Name, Type, {Day, {4, 4, 4}}, Middle),
  M5 = pollution:addValue(M4, Name, Type, {Day, {5, 5, 5}}, Middle + 1),
  M6 = pollution:addValue(M5, Name, Type, {Day, {6, 6, 6}}, Middle + 2),

  M7 = pollution:addStation(M6, NameOne, LocationOne),
  M8 = pollution:addValue(M7, NameOne, Type, {Day, {7, 7, 7}}, Middle - 2),
  M9 = pollution:addValue(M8, NameOne, Type, {Day, {8, 8, 8}}, Middle - 1),
  M10 = pollution:addValue(M9, NameOne, Type, {Day, {9, 9, 9}}, Middle),
  M11 = pollution:addValue(M10, NameOne, Type, {Day, {10, 10, 10}}, Middle + 1),
  M12 = pollution:addValue(M11, NameOne, Type, {Day, {11, 11, 11}}, Middle + 2),

  ?assertError(badarg, pollution:getDailyMean(M12, OtherType, Day)).


getMaximumVariationStation_returns_valid_station_from_a_few_stations_test() ->
  Name = "0", NameOne = "1", NameTwo = "2",
  Location = {0, 0}, LocationOne = {1, 1}, LocationTwo = {2, 2},
  Day = {2001, 01, 01},
  Type = "PM10",
  Middle = 100.0,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, {Day, {2, 2, 2}}, Middle - 10),
  M3 = pollution:addValue(M2, Name, Type, {Day, {3, 3, 3}}, Middle - 1),
  M4 = pollution:addValue(M3, Name, Type, {Day, {4, 4, 4}}, Middle),
  M5 = pollution:addValue(M4, Name, Type, {Day, {5, 5, 5}}, Middle + 1),
  M6 = pollution:addValue(M5, Name, Type, {Day, {6, 6, 6}}, Middle + 20),

  M7 = pollution:addStation(M6, NameOne, LocationOne),
  M8 = pollution:addValue(M7, NameOne, Type, {Day, {7, 7, 7}}, Middle - 3),
  M9 = pollution:addValue(M8, NameOne, Type, {Day, {8, 8, 8}}, Middle - 1),
  M10 = pollution:addValue(M9, NameOne, Type, {Day, {9, 9, 9}}, Middle),
  M11 = pollution:addValue(M10, NameOne, Type, {Day, {10, 10, 10}}, Middle + 1),
  M12 = pollution:addValue(M11, NameOne, Type, {Day, {11, 11, 11}}, Middle + 2),

  M13 = pollution:addStation(M12, NameTwo, LocationTwo),
  M14 = pollution:addValue(M13, NameTwo, Type, {Day, {12, 12, 12}}, Middle - 2),
  M15 = pollution:addValue(M14, NameTwo, Type, {Day, {13, 13, 13}}, Middle - 1),
  M16 = pollution:addValue(M15, NameTwo, Type, {Day, {14, 14, 14}}, Middle),
  M17 = pollution:addValue(M16, NameTwo, Type, {Day, {15, 15, 15}}, Middle + 1),
  M18 = pollution:addValue(M17, NameTwo, Type, {Day, {16, 16, 16}}, Middle + 2),

  ?assertEqual(
    {30.0, #station{name = Name, location = Location}},
    pollution:getMaximumVariationStation(M18, Type)).


getMaximumVariationStation_error_if_monitor_empty_test() ->
  Type = "PM10",

  M0 = pollution:createMonitor(),

  ?assertError(function_clause, pollution:getMaximumVariationStation(M0, Type)).


getMaximumVariationStation_returns_valid_station_on_tie_test() ->
  Name = "0", NameOne = "1", NameTwo = "2",
  Location = {0, 0}, LocationOne = {1, 1}, LocationTwo = {2, 2},
  Day = {2001, 01, 01},
  Type = "PM10",
  Middle = 100.0,

  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(M0, Name, Location),
  M2 = pollution:addValue(M1, Name, Type, {Day, {2, 2, 2}}, Middle - 2),
  M3 = pollution:addValue(M2, Name, Type, {Day, {3, 3, 3}}, Middle - 1),
  M4 = pollution:addValue(M3, Name, Type, {Day, {4, 4, 4}}, Middle),
  M5 = pollution:addValue(M4, Name, Type, {Day, {5, 5, 5}}, Middle + 1),
  M6 = pollution:addValue(M5, Name, Type, {Day, {6, 6, 6}}, Middle + 2),

  M7 = pollution:addStation(M6, NameOne, LocationOne),
  M8 = pollution:addValue(M7, NameOne, Type, {Day, {7, 7, 7}}, Middle - 2),
  M9 = pollution:addValue(M8, NameOne, Type, {Day, {8, 8, 8}}, Middle - 1),
  M10 = pollution:addValue(M9, NameOne, Type, {Day, {9, 9, 9}}, Middle),
  M11 = pollution:addValue(M10, NameOne, Type, {Day, {10, 10, 10}}, Middle + 1),
  M12 = pollution:addValue(M11, NameOne, Type, {Day, {11, 11, 11}}, Middle + 2),

  M13 = pollution:addStation(M12, NameTwo, LocationTwo),
  M14 = pollution:addValue(M13, NameTwo, Type, {Day, {12, 12, 12}}, Middle - 2),
  M15 = pollution:addValue(M14, NameTwo, Type, {Day, {13, 13, 13}}, Middle - 1),
  M16 = pollution:addValue(M15, NameTwo, Type, {Day, {14, 14, 14}}, Middle),
  M17 = pollution:addValue(M16, NameTwo, Type, {Day, {15, 15, 15}}, Middle + 1),
  M18 = pollution:addValue(M17, NameTwo, Type, {Day, {16, 16, 16}}, Middle + 2),

  ?assertMatch(
    {4.0, #station{}},
    pollution:getMaximumVariationStation(M18, Type)).
