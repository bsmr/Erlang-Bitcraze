%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2014, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created :  3 Aug 2014 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(joystick_example).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {handle = unknown}).

%% API
-export([run/0]).
-export([start/1, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

run() ->
    %%observer:start(),
    Pathname = "/dev/input/js0",
    io:format("~n~n~n"),
    io:format("Enter:~n> {ok, Pid} = joystick_example:start(~p).~n", [Pathname]),
    io:format("Enter:~n> {ok, Pid} = joystick:attach(~p).~n", [Pathname]),
    io:format("~n~n~n"),
    ok.
%%    {ok, Pid} = start(Pathname),
%%    timer:sleep(5 * 1000),
%%    stop(Pid),
%%    init:stop().

start(Pathname) ->
    io:format("~n*** ~p:start()~n", [?MODULE]),
    gen_server:start({local, ?SERVER}, ?MODULE, [Pathname], []).

stop(Pid) ->
    io:format("~n*** ~p:stop()~n", [?MODULE]),
    gen_server:call(Pid, shutdown).

%% gen_server callbacks
init([Pathname]) ->
    io:format("~n*** ~p:init([Pathname]:~p)~n", [?MODULE, Pathname]),
    case joystick:attach(Pathname) of
	{ok, Handle} ->
	    joystick_info(Handle),
	    {ok, #state{ handle = Handle }};
	{error, Error} ->
	    {stop, Error}
    end.

joystick_info(Handle) ->
    {ok, Name}    = joystick:name(Handle),
    {ok, Version} = joystick:version(Handle),
    {ok, Axes}    = joystick:axes(Handle),
    {ok, Buttons} = joystick:buttons(Handle),
    io:format("~n*** Joystick: Name:~p, Version:~p, Axes:~p, Buttons:~p ***~n",
	      [Name, Version, Axes, Buttons]),
    ok.

handle_call(shutdown, From, #state{ handle = Handle } = State ) ->
    io:format("~n*** ~p:handle_call(shutdown, From:~p, State:~p)~n",
	      [?MODULE, From, State]),
    case joystick:detach(Handle) of
	ok ->
	    NewState = State#state{ handle = unknown },
	    {reply, ok, NewState};
	{error, Error} ->
	    {reply, {error, Error}, State}
    end; 
handle_call(Request, From, State) ->
    io:format("~n*** ~p:handle_call(Request:~p, From:~p, State:~p)~n",
	      [?MODULE, Request, From, State]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    io:format("~n*** ~p:handle_cast(Msg:~p, State:~p)~n",
	      [?MODULE, Msg, State]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("*** ~p:handle_info(Info:~p, State:~p)~n",
	      [?MODULE, Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
