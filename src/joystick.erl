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
-on_load(init/0).

%% API
-export([example/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

example() ->
    Pathname = "/dev/input/js0",
    case jsnif_open(Pathname) of
	{ok, FD} ->
	    case jsnif_read(FD) of
		{ok, no_event} ->
		    io:format("event: none~n");
		{ok, Time, Value, Type, Number} ->
		    io:format("event:~p/~p/~p/~p~n", [Time, Value, Type, Number]);
		{error, Errno} ->
		    io:format("Error: ~p~n", [Errno])
	    end,
	    jsnif_close(FD);
	{error, Errno} ->
	    io:format("Error: ~p~n", [Errno])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% NIF init and private NIFs
%%%===================================================================

init() ->
    ok = erlang:load_nif("../priv/joysticknif", 0).

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
