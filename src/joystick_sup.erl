%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2014, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created :  3 Aug 2014 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(joystick_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Pathname) ->
    supervisor:start_child(?SERVER, [{pathname, Pathname}]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->

    SupFlags = {simple_one_for_one, 0, 1},

    JoystickServer = {joystick_srv,
		      {joystick_srv, start_link, []},
		      temporary, brutal_kill, worker,
		      [joystick_srv]},

    {ok, {SupFlags, [JoystickServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
