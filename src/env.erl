-module(env).

-import(bad, [start/1, stop/2]).

-export([start/0, stop/1]).

-define(NUM_BAD, 5).

start() ->
    io:format("ENV: Started with PID: ~p~n", [self()]),
    BADList = startBADs(?NUM_BAD), 
    loop(BADList).

startBADs(0) -> [];
startBADs(N) when N > 0 ->
    {BADID, PingTimer} = bad:start(self()),
    io:format("ENV: Started BAD with ID: ~p~n", [BADID]),
    [{BADID, PingTimer}] ++ startBADs(N-1).
        
stop(BADList) -> self() ! {stop}.
  

loop(BADList) ->
    receive
        {From, {ping, Lat, Lang}} ->
            io:format("ENV: Received Ping from BAD ~p~n", [From]),
            lists:foreach(fun({BADID, PingTimer}) ->
                BADID ! {From, {ping, Lat, Lang}}
                end, BADList),
            loop(BADList);
        {stop} ->
            io:format("ENV: Received Stop signal~n"),
            lists:foreach(fun({BADID, PingTimer}) ->
                bad:stop(BADID, PingTimer)
                end, BADList)
    end.
