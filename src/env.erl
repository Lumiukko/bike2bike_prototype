-module(env).

-import(bad, [start/1, stop/2]).

% For testing purposes, export all.
-compile(export_all).

% For live system, only export API functions.
%-export([start/0, stop/1]).

-define(NUM_BAD, 5).



%==============================================================================
% Start API function to start the Environment and the BADs.
%==============================================================================

start() ->
    ENVID = spawn(fun() -> init() end),
    io:format("ENV: Started with PID: ~p~n", [ENVID]),
    BADList = startBADs(?NUM_BAD, ENVID),
    ENVID ! {badlist, BADList},
    ENVID.



%==============================================================================
% Helper to receive the list of BADs *after* the Environment has been created.
%==============================================================================

init() ->
    receive
        {badlist, BADList} ->
            io:format("ENV: Received BAD List with ~p BAD IDs.~n", [length(BADList)]),
            loop(BADList)
    end.



%==============================================================================
% Recursive Helper to start a number of BADs.
%==============================================================================

startBADs(0, _) -> [];
startBADs(N, ENVID) when N > 0 ->
    {BADID, PingTimer} = bad:start(ENVID),
    io:format("ENV: Started BAD with ID: ~p~n", [BADID]),
    [{BADID, PingTimer}] ++ startBADs(N-1, ENVID).



%==============================================================================
% Stop API function to stop the Environment and the BADs.
%==============================================================================

stop(ENVID) -> ENVID ! {stop}.



%==============================================================================
% Main loop of the Environment, receiving BAD pings and Stop signal.
%==============================================================================

loop(BADList) ->
    receive
        {From, {ping, {Lat, Lang}}} ->
            io:format("ENV: Received Ping from BAD ~p~n", [From]),
            lists:foreach(fun({BADID, _}) ->
                BADID ! {From, {ping, {Lat, Lang}}}
                end, BADList),
            loop(BADList);
        {stop} ->
            io:format("ENV: Received Stop signal~n"),
            lists:foreach(fun({BADID, PingTimer}) ->
                bad:stop(BADID, PingTimer)
                end, BADList)
    end.



%==============================================================================
% Function to calculate the disance between two geographical coordinates.
%==============================================================================

distance(LatA, LongA, LatB, LongB) ->
    R = 6378.137,                               % Radius of earth in KM
    DLat = (LatB - LatA) * math:pi() / 180,
    DLong = (LongB - LongA) * math:pi() / 180,
    A = math:sin(DLat/2) * math:sin(DLat/2) + math:cos(LatA * math:pi() / 180) * math:cos(LatB * math:pi() / 180) * math:sin(DLong/2) * math:sin(DLong/2),
    C = 2 * math:atan2(math:sqrt(A), math:sqrt(1-A)),
    D = R * C,
    D * 1000.                                    % Distance in Meters



%==============================================================================
% Test function for the distance function, calculating the distance between 
% DIKU (Universitetsparken 1, Copenhagen) Front and Back entrance.
%==============================================================================

distUP1_Front_Back() ->
    distance(55.702089, 12.561057, 55.702083, 12.561271).


