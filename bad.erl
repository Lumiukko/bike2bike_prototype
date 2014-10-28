% Bicycle attached device prototype

-module(bad).
-export([start/1, stop/2]).
-import(timer, [apply_interval/4,cancel/1]).

-define(PING_INTERVAL, 5000).

% starting new BAD
% ENV: environment process ID
start(ENV) ->
	% spawning new BAD process
	BAD = spawn(fun() -> bad_loop({undefined, undefined}) end),
	% setting time to ping
	{ok,PingTimer} = timer:apply_interval(?PING_INTERVAL, bad, ping, [ENV, BAD]),
	{BAD,PingTimer}.
	
% stopping BAD and ping timer
stop(PID, PingTimer) ->
	rpc(PID, stop),
	timer:cancel(PingTimer).

ping(ENV, BAD) ->
	BAD ! {ENV, time2ping}.
	
	
bad_loop({Lat, Long}) ->
	receive
		{From, stop} ->
			io:format("BAD ~p: BAD shutting down~n", [self()]),
			reply_ok(From);
		{ENV, time2ping} ->
			ENV ! {self(), ping, {Lat, Long}},
			bad_loop({Lat, Long})
	end.
			
			
			
% Standard communication functions

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
		{Pid, Response} ->
			Response
    end.

reply(To, Msg) ->
    To ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).