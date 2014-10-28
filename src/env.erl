-module(env).

-import(bad, [start/1, stop/2]).

-export([start/0, stop/1]).

-define(NUM_BAD, 5).

start() ->
	ENVID = spawn(fun() -> init() end),
    io:format("ENV: Started with PID: ~p~n", [ENVID]),
	BADList = startBADs(?NUM_BAD, ENVID),
	ENVID ! {badlist, BADList},
	ENVID.

init() ->
	receive
		{badlist, BADList} ->
			io:format("ENV: Received BAD List with ~p BAD IDs.~n", [length(BADList)]),
			loop(BADList)
	end.
	

startBADs(0, _) -> [];
startBADs(N, ENVID) when N > 0 ->
    {BADID, PingTimer} = bad:start(ENVID),
    io:format("ENV: Started BAD with ID: ~p~n", [BADID]),
    [{BADID, PingTimer}] ++ startBADs(N-1, ENVID).
        
stop(ENVID) -> ENVID ! {stop}.
  

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
