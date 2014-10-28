-module(env).

%-compile(export_all).

-export([start/0]).

-define(NUM_BAD, 5).

start() ->
    io:format("ENV: Started with PID: ~p~n", [self()]),
    BADList = startBADs(?NUM_BAD), 
    loop(BADList).

startBADs(0) -> [].
startBADs(N) -> 
    {BADID, PingTimer} = bad:start(self()),
    io:format("ENV: Started BAD with ID: ~p~n", [BADID]),
    [{BADID, PingTimer}] ++ startBADs(N-1).
        
stop() -> undefined.    
  

loop(BADList) ->
    receive
        {From, {ping, Lat, Lang}} ->
            io:format("ENV: Received Ping from BAD ~p~n", [From]),
            lists:foreach(fun({BADID, PingTimer}) ->
                BADID ! {From, {ping, Lat, Lang}}
                end, BADList)
    end.
