% Bicycle attached device prototype

-module(bad).
-export([start/1, stop/2]).
-import(timer, [apply_interval/4,cancel/1]).

-define(PING_INTERVAL, 5000).

% starting new BAD
% ENV: environment process ID
start(ENV) ->
	% spawning new BAD process
	BAD = spawn(fun() -> bad_loop(undefined, {undefined, undefined}, dict:new()) end),
	% setting time to ping
	{ok,PingTimer} = timer:apply_interval(?PING_INTERVAL, bad, ping, [ENV, BAD]),
	{BAD,PingTimer}.
	
% stopping BAD and ping timer
stop(PID, PingTimer) ->
	rpc(PID, stop),
	timer:cancel(PingTimer).

ping(ENV, BAD) ->
	BAD ! {ENV, time2ping}.
	
	
bad_loop(MPA, {Lat, Long}, BADList) ->
	receive
		% stopping BAD
		{From, stop} ->
			io:format("BAD ~p: BAD shutting down~n", [self()]),
			reply_ok(From);
		% informing BAD that it is time to ping for new BADs
		{ENV, time2ping} ->
			ENV ! {self(), ping, {Lat, Long}},
			bad_loop(MPA, {Lat, Long}, BADList);
		% receiving ping acknowledgement by other BAD
		{From, pingACK, {OtherLat, OtherLong}} ->
			bad_loop(MPA, {Lat, Long}, dict:store(From, {OtherLat, OtherLong}, BADList));
		% receiving ping by other BAD
		{From, ping, {OtherLat, OtherLong}} ->
			reply(From, {pingACK, {Lat, Long}}),
			bad_loop(MPA, {Lat, Long}, dict:store(From, {OtherLat, OtherLong}, BADList));
		% bluetooth registration by an MPA, check whether already paired
		{From, register} ->
			case MPA of
				undefined ->
					reply_ok(From),
					bad_loop(From, {Lat, Long}, BADList);
				_else ->
					reply_fail(From)
			end;
		% unregistration msg by paired MPA
		{MPA, unregister} ->
			reply_ok(MPA),
			bad_loop(undefined, {Lat, Long}, BADList);
		% unregistration msg by unknown MPA, fail
		{Other, unregister} ->
			reply_fail(Other),
			bad_loop(MPA, {Lat, Long}, BADList)
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

reply_fail(From) ->
    reply(From, fail).