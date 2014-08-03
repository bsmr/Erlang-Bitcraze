%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2014, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created :  3 Aug 2014 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(joystick_srv).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([shutdown/1]).
-export([name/1, version/1, axes/1, buttons/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pathname            = unknown,
		fd                  = unknown,
		timeout_unspecified = 1000,
		timeout_noevent     = 100,
		timeout_event       = 0,
		timeout_error       = 5000}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    io:format("~n*** ~p:start_link(Args:~p)~n", [?MODULE, Args]),
    gen_server:start_link(?MODULE, [Args], []).

shutdown(Handle) ->
    gen_server:call(Handle, shutdown).

name(Handle) ->
    gen_server:call(Handle, name).

version(Handle) ->
    gen_server:call(Handle, version).

axes(Handle) ->
    gen_server:call(Handle, axes).

buttons(Handle) ->
    gen_server:call(Handle, buttons).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{pathname, Pathname}] = Args) ->
    io:format("~n*** ~p:init(Args:~p)~n", [?MODULE, Args]),
    case joystick:jsnif_open(Pathname) of
	{ok, FD} ->
	    {ok, #state{pathname = Pathname, fd = FD}, 0};
	{error, Errno} ->
	    {stop, {error, Errno}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(name, _From, #state{fd = FD} = State) ->
    {reply, joystick:jsnif_name(FD), State};
handle_call(version, _From, #state{fd = FD} = State) ->
    {reply, joystick:jsnif_version(FD), State};
handle_call(axes, _From, #state{fd = FD} = State) ->
    {reply, joystick:jsnif_axes(FD), State};
handle_call(buttons, _From, #state{fd = FD} = State) ->
    {reply, joystick:jsnif_buttons(FD), State};

handle_call(shutdown, From, #state{ fd = unknown } = State) ->
    io:format("~n*** ~p:handle_call(shutdown, From:~p, State:~p)~n",
	      [?MODULE, From, State]),
    {stop, ok, State};
handle_call(shutdown, From, #state{ fd = FD } = State) ->
    io:format("~n*** ~p:handle_call(shutdown, From:~p, State:~p)~n",
	      [?MODULE, From, State]),
    case joystick:jsnif_close(FD) of
	{error, Errno} ->
	    {stop, ok, {error, Errno}, State};
	ok ->
	    {stop, ok, State#state{ fd = unknown }}
    end;

handle_call(Request, From, State) ->
    io:format("~n*** ~p:handle_call(Request:~p, From:~p, State:~p)~n",
	      [?MODULE, Request, From, State]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("~n*** ~p:handle_cast(Msg:~p, State:~p)~n",
	      [?MODULE, Msg, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(timeout, #state{ fd = unknown, timeout_unspecified = TOnospec } = State) ->
    {noreply, State, TOnospec};

handle_info(timeout, #state{ fd=FD, timeout_noevent=TOnoevent, timeout_event=TOevent, timeout_error=TOerror } = State) ->
    case joystick:jsnif_read(FD) of
	{ok, noevent} ->
	    %%io:format("~n### Joystick Event: no data ###~n"),
	    {noreply, State, TOnoevent};
	{ok, _Time, Value, Type, Number} ->
	    %%io:format("~n### Joystick Event: Time:~p, Value:~p, Type:~p, Number:~p ###~n", [Time, Value, Type, Number]),
	    case Type of
		1 ->
		    %%io:format("### Button: ~p/~p~n", [Number, Value]),
		    ok;
		2 ->
		    %%io:format("### Axis: ~p/~p~n", [Number, Value]),
		    ok;
		129 ->
		    %%io:format("### Button/Init: ~p/~p~n", [Number, Value]),
		    ok;
		130 ->
		    %%io:format("### Axis/Init: ~p/~p~n", [Number, Value]),
		    ok;
		_ ->
		    io:format("### Joystick: ~p/~p/~p~n", [Type, Number, Value])
	    end,
	    {noreply, State, TOevent};
	{error, Errno} ->
	    io:format("~n### Joystick Error ~p ###~n", [Errno]),
	    {noreply, State, TOerror}
    end;

handle_info(Info, State) ->
    io:format("~n*** ~p:handle_info(Info:~p, State:~p)~n",
	      [?MODULE, Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{fd = FD}) ->
    case FD of
	unknown ->
	    ok;
	_ ->
	    joystick:jsnif_close(FD) 
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
