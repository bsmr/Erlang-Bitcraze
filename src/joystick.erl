%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2014, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 29 Jul 2014 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(joystick).
-on_load(initnif/0).

%% API
-export([start/0, attach/1, detach/1]).
-export([name/1, version/1, axes/1, buttons/1]).
-export([information/1]).

%% API for joystick NIF
-export([jsnif_open/1, jsnif_close/1, jsnif_read/1,
	 jsnif_name/1, jsnif_version/1, jsnif_axes/1, jsnif_buttons/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start() ->
    application:start(joystick).

attach(Pathname) ->
    case joystick_sup:start_child(Pathname) of
	{ok, Child} ->
	    {ok, Child};
	{error, Reason} ->
	    {error, Reason}
    end.

detach(Handle) ->
    joystick_srv:shutdown(Handle).

name(Handle) ->
    joystick_srv:name(Handle).

version(Handle) ->
    joystick_srv:version(Handle).

axes(Handle) ->
    joystick_srv:axes(Handle).

buttons(Handle) ->
    joystick_srv:buttons(Handle).

information(Handle) ->
    {ok, Name}    = joystick:name(Handle),
    {ok, Version} = joystick:version(Handle),
    {ok, Axes}    = joystick:axes(Handle),
    {ok, Buttons} = joystick:buttons(Handle),
    {ok, [{name, Name}, {version, Version}, {axes, Axes}, {buttons, Buttons}]}.

%% example() ->
%%     Pathname = "/dev/input/js0",
%%     case jsnif_open(Pathname) of
%% 	{ok, FD} ->
%% 	    case jsnif_read(FD) of
%% 		{ok, no_event} ->
%% 		    io:format("event: none~n");
%% 		{ok, Time, Value, Type, Number} ->
%% 		    io:format("event:~p/~p/~p/~p~n", [Time, Value, Type, Number]);
%% 		{error, Errno} ->
%% 		    io:format("Error: ~p~n", [Errno])
%% 	    end,
%% 	    jsnif_close(FD);
%% 	{error, Errno} ->
%% 	    io:format("Error: ~p~n", [Errno])
%%     end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% NIF init and private NIFs
%%%===================================================================

initnif() ->
    ok = erlang:load_nif("priv/joystick", 0).

jsnif_open(_Pathname) ->
    exit(joysticknif_not_loaded).
jsnif_close(_FD) ->
    exit(joysticknif_not_loaded).
jsnif_read(_FD) ->
    exit(joysticknif_not_loaded).
jsnif_name(_FD) ->
    exit(joysticknif_not_loaded).
jsnif_version(_FD) ->
    exit(joysticknif_not_loaded).
jsnif_axes(_FD) ->
    exit(joysticknif_not_loaded).
jsnif_buttons(_FD) ->
    exit(joysticknif_not_loaded).

%%%===================================================================
%%% End Of File
%%%===================================================================
