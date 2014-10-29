% Bicycle attached device prototype

-module(bad).
-export([start/1, stop/2, ping/2]).
-import(timer, [apply_interval/4,cancel/1]).

-define(PING_INTERVAL, 5000).
-define(ACCIDENT_TIMEOUT, 3000).

% starting new BAD
% ENV: environment process ID
start(ENV) ->
	% spawning new BAD process
	BAD = spawn(fun() -> bad_loop(undefined, {undefined, undefined}, dict:new()) end),
	io:format("BAD ~p: BAD started~n", [BAD]),
	% setting time to ping
	{ok,PingTimer} = timer:apply_interval(?PING_INTERVAL, bad, ping, [ENV, BAD]),
	{BAD,PingTimer}.
	
% stopping BAD and ping timer
stop(PID, PingTimer) ->
	rpc(PID, stop),
	timer:cancel(PingTimer).

ping(ENV, BAD) -> BAD ! {ENV, time2ping}.
	
	
bad_loop(MPA, {Lat, Long}, BADList) ->
	Self = self(),
	receive
		% stopping BAD
		{From, stop} ->
			io:format("BAD ~p: BAD shutting down~n", [Self]),
			reply_ok(From);
		% ENV informing BAD that it is time to ping for new BADs
		{ENV, time2ping} ->
			io:format("BAD ~p: Ping broadcast~n", [Self]),
			reply(ENV, {ping, {Lat, Long}}),
			bad_loop(MPA, {Lat, Long}, BADList);
		{event, accident} ->
			case MPA of
				undefined ->
					%send msg to other BADs
					io:format("BAD ~p: Accident recognized. MPA non connected, calling other BADs. ~n", [Self]),
					{List, _} = dict:to_list(BADList),
					bad_accident_loop(MPA, {Lat, Long}, BADList, List);
				MPA ->
					%send msg to MPA
					io:format("BAD ~p: Accident recognized. MPA is informed. ~n", [Self]),
					bad_loop(MPA, {Lat, Long}, BADList)
			end;
		{From, event, accident_inform, {Lat, Long}} ->
			case MPA of
				undefined ->
					%send msg to other BADs
					io:format("BAD ~p: Informed about accident of BAD ~p, not connected to MPA though. ~n", [Self, From]),
					From ! {self(), event, accident_not_reported},
					bad_loop(MPA, {Lat, Long}, BADList);
				MPA ->
					%send msg to MPA
					io:format("BAD ~p: Informed about accident of BAD ~p. MPA is informed. ~n", [Self, From]),
					From ! {self(), event, accident_reported},
					bad_loop(MPA, {Lat, Long}, BADList)
			end;
		% ENV informing BAD that it is time to ping for new BADs
		{setloc, {NewLat, NewLong}} ->
			io:format("BAD ~p: BAD is at a new position~n", [Self]),
			bad_loop(MPA, {NewLat, NewLong}, BADList);
		% receiving ping acknowledgement by other BAD
		{From, {pingACK, {OtherLat, OtherLong}}} ->
			io:format("BAD ~p: Received ping ACK from ~p~n", [self(),From]),
			bad_loop(MPA, {Lat, Long}, dict:store(From, {OtherLat, OtherLong}, BADList));
		% receiving own ping, disregard it
		{Self, {ping, _}} ->
			bad_loop(MPA, {Lat, Long}, BADList);
		% receiving ping by other BAD
		{From, {ping, {OtherLat, OtherLong}}} ->
			io:format("BAD ~p: Received ping from ~p~n", [self(), From]),
			reply(From, {pingACK, {Lat, Long}}),
			bad_loop(MPA, {Lat, Long}, dict:store(From, {OtherLat, OtherLong}, BADList));
		% bluetooth registration by an MPA, check whether already paired
		{From, register} ->
			case MPA of
				undefined ->
					io:format("BAD ~p: BAD paired with MPA ~p~n", [Self, From]),
					reply_ok(From),
					bad_loop(From, {Lat, Long}, BADList);
				_else ->
					io:format("BAD ~p: Pairing with ~p failed: Already paired with MPA ~p~n", [Self, From,MPA]),
					reply_fail(From)
			end;
		% unregistration msg by paired MPA
		{MPA, unregister} ->
			io:format("BAD ~p: BAD unpairs from MPA ~p~n", [Self, MPA]),
			reply_ok(MPA),
			bad_loop(undefined, {Lat, Long}, BADList);
		% unregistration msg by unknown MPA, fail
		{Other, unregister} ->
			io:format("BAD ~p: BAD unpairing with MPA ~p failed: Unknown MPA~n", [Self, Other]),
			reply_fail(Other),
			bad_loop(MPA, {Lat, Long}, BADList);
		{From, Msg} ->
			io:format("BAD ~p: Received unknown message from ~p: ~p~n", [Self, From, Msg]),
			bad_loop(MPA, {Lat, Long}, BADList)
	end.

bad_accident_loop(MPA, {Lat, Long}, BADList, []) ->
	io:format("BAD ~p: Accident report: None of the known BADs answered/can report the accident, try again~n", [self()]),
	{List, _} = dict:to_list(BADList),
	bad_accident_loop(MPA, {Lat, Long}, BADList, List);
bad_accident_loop(MPA, {Lat, Long}, BADList, [CurrentBAD|List]) ->
	Self = self(),
	CurrentBAD ! {Self, event, accident_inform, {Lat, Long}},
	receive
		{From, event, accident_reported} ->
			io:format("BAD ~p: Accident reported by BAD ~p~n", [Self, From]),
			bad_loop(MPA, {Lat, Long}, BADList);
		{From, event, accident_not_reported} ->
			io:format("BAD ~p: Accident report: BAD ~p does not have an MPA either, contacting next known BAD~n", [Self, From]),
			bad_accident_loop(MPA, {Lat, Long}, BADList, List);
		{setloc, {NewLat, NewLong}} ->
			io:format("BAD ~p: BAD is at a new position~n", [Self]),
			bad_accident_loop(MPA, {NewLat, NewLong}, BADList, List);
		% receiving ping acknowledgement by other BAD
		{From, {pingACK, {OtherLat, OtherLong}}} ->
			io:format("BAD ~p: Received ping ACK from ~p~n", [Self,From]),
			bad_accident_loop(MPA, {Lat, Long}, dict:store(From, {OtherLat, OtherLong}, BADList), List);
		% receiving own ping, disregard it
		{Self, {ping, _}} ->
			bad_accident_loop(MPA, {Lat, Long}, BADList, List);
		% receiving ping by other BAD
		{From, {ping, {OtherLat, OtherLong}}} ->
			io:format("BAD ~p: Received ping from ~p~n", [Self, From]),
			reply(From, {pingACK, {Lat, Long}}),
			bad_accident_loop(MPA, {Lat, Long}, dict:store(From, {OtherLat, OtherLong}, BADList), [List|From])
		after 
			?ACCIDENT_TIMEOUT ->
			io:format("BAD ~p: Accident report: BAD ~p did not answer, contacting next known BAD~n", [Self, CurrentBAD]),
			bad_accident_loop(MPA, {Lat, Long}, BADList, List)
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