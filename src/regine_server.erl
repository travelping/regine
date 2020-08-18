%% Copyright 2011-2017, Travelping GmbH <info@travelping.com>

%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(regine_server).
-behaviour(gen_server).

-export([start/2, start/3, start_link/2, start_link/3, register/4, unregister/3, update/4,
         unregister_pid/2, unregister_pid/3, lookup_pid/2,
         call/2, call/3, cast/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------------------------------
%% -- Behavior

-callback init(Args :: term()) ->
    {ok, State :: term()} |
    {stop, Reason :: term()}.

-callback handle_register(Pid :: pid(), Key :: term(), Args :: term(), State :: term()) ->
    {ok, Keys :: [term()], NewState :: term()} |
    {error, Reason :: term()}.

-callback handle_unregister(Key :: term(), Args :: term(), State :: term()) ->
    {Keys :: [term()], NewState :: term()}.

-callback handle_update(OldKey :: term(), NewKey :: term(), Args :: term(), State :: term()) ->
    {Pids :: [pid()], NewState :: term()}.

-callback handle_pid_remove(Pid :: pid(), Keys :: [term()], State :: term()) ->
    NewState :: term().

-callback handle_death(Pid :: pid(), Reason :: term(), State :: term()) ->
    NewState :: term().

-callback terminate(Reason :: term(), State :: term()) ->
    ok.

-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_info(Info :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-optional_callbacks([handle_call/3, handle_cast/2, handle_info/2, handle_update/4]).

%% ------------------------------------------------------------------------------------------
%% -- API

start(CallbackModule, InitArgs) ->
    gen_server:start(?MODULE, {CallbackModule, InitArgs}, []).
start(RegSpec, CallbackModule, InitArgs) ->
    gen_server:start(RegSpec, ?MODULE, {CallbackModule, InitArgs}, []).

start_link(CallbackModule, InitArgs) ->
    gen_server:start_link(?MODULE, {CallbackModule, InitArgs}, []).
start_link(RegSpec, CallbackModule, InitArgs) ->
    gen_server:start_link(RegSpec, ?MODULE, {CallbackModule, InitArgs}, []).

register(Server, Pid, Key, OtherArgs) when is_pid(Pid) ->
    gen_server:call(Server, {register, Pid, Key, OtherArgs});
register(_, _, _, _) ->
    error(badarg).

unregister(Server, Key, OtherArgs) ->
    gen_server:call(Server, {unregister_key, Key, OtherArgs}).

unregister_pid(Server, Pid) when is_pid(Pid) ->
    gen_server:call(Server, {unregister_pid, Pid}).

unregister_pid(Server, Pid, Key) when is_pid(Pid) ->
    gen_server:call(Server, {unregister_pid_key, Pid, Key}).

update(Server, OldKey, NewKey, OtherArgs) ->
    gen_server:call(Server, {update_key, OldKey, NewKey, OtherArgs}).

lookup_pid(Server, Pid) when is_pid(Pid) ->
    gen_server:call(Server, {lookup_pid, Pid});
lookup_pid(_, _) ->
    error(badarg).

call(Server, Call) ->
    gen_server:call(Server, {cb, Call}).

call(Server, Call, Timeout) ->
    gen_server:call(Server, {cb, Call}, Timeout).

cast(Server, Msg) ->
    gen_server:cast(Server, {cb, Msg}).

stop(Server) ->
    gen_server:call(Server, stop_regine_server).

%% ------------------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {
    mod      :: module(),
    modstate :: term(),
    pidmap   :: map()
}).

init({CBMod, CBArgs}) ->
    process_flag(trap_exit, true),

    case CBMod:init(CBArgs) of
        {ok, CBState} ->
            {ok, #state{pidmap = #{}, mod = CBMod, modstate = CBState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call({register, Pid, Key, OtherArgs}, _From,
	    State0 = #state{pidmap = PidMap}) ->
    case handle_register(Pid, Key, OtherArgs, State0) of
        {ok, DelKeys, State1} ->
            link(Pid), %% TODO: handle case when registered pid is not alive
            DKSet     = ordsets:from_list(DelKeys),
            NewPidMap = maps:update_with(Pid,
					 fun (Old) ->
						 ordsets:union(DKSet, Old)
					 end, DelKeys, PidMap),
            NewState  = State1#state{pidmap = NewPidMap},
            {reply, ok, NewState};
        ErrorReply = {error, _} ->
            {reply, ErrorReply, State0}
    end;

handle_call({unregister_key, Key, OtherArgs}, _From,
	    State0 = #state{pidmap = PidMap}) ->
    {RegPids, State1} = handle_unregister(Key, OtherArgs, State0),
    NewPidMap = lists:foldl(fun (Pid, Acc) ->
				    remove_pid_key(Pid, Key, Acc)
			    end, PidMap, RegPids),
    NewState  = State1#state{pidmap = NewPidMap},
    {reply, ok, NewState};

handle_call({update_key, OldKey, NewKey, OtherArgs}, _From,
	    State0 = #state{pidmap = PidMap}) ->
    {RegPids, State1} = handle_update(OldKey, NewKey, OtherArgs, State0),
    NewPidMap = lists:foldl(fun (Pid, Acc) ->
				    update_pid_key(Pid, OldKey, NewKey, Acc)
			    end, PidMap, RegPids),
    NewState = State1#state{pidmap = NewPidMap},
    {reply, ok, NewState};

handle_call({unregister_pid, Pid}, _From, State0 = #state{pidmap = PidMap}) ->
    case PidMap of
	#{Pid := Keys} ->
            unlink(Pid),
	    State1 = handle_pid_remove(Pid, Keys, State0),
	    State  = State1#state{pidmap = maps:remove(Pid, PidMap)},
            {reply, ok, State};
        _ ->
            {reply, {error, unknown_pid}, State0}
    end;

handle_call({unregister_pid_key, Pid, Key}, _From, State0 = #state{pidmap = PidMap}) ->
    case PidMap of
	#{Pid := _} ->
	    State1 = handle_pid_remove(Pid, [Key], State0),
            State  = State1#state{pidmap = remove_pid_key(Pid, Key, PidMap)},
	    {reply, ok, State};
        _ ->
            {reply, {error, unknown_pid}, State0}
    end;

handle_call({lookup_pid, Pid}, _From, State = #state{pidmap = PidMap}) ->
    Keys = maps:get(Pid, PidMap, []),
    {reply, Keys, State};

handle_call({cb, Call}, From, State) ->
    case catch cb(handle_call, [Call, From], State) of
        {reply, Reply, CBState} ->
	    {reply, Reply, cb_state(CBState, State)};
        {noreply, CBState} ->
	    {noreply, cb_state(CBState, State)};
        {stop, Reason, Reply, CBState} ->
	    {reply, Reason, Reply, cb_state(CBState, State)};
        {stop, Reason, CBState} ->
	    {stop, Reason, cb_state(CBState, State)};
        Other ->
            {reply, Other, State}
    end;

handle_call(stop_regine_server, _From, State) ->
    {stop, normal, ok,  State};

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast({cb, Msg}, State)->
    case catch cb(handle_cast, [Msg], State) of
        {noreply, CBState} ->
            {noreply, cb_state(CBState, State)};
        {stop, Reason, CBState} ->
            {stop, Reason, cb_state(CBState, State)};
        _ ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason},
	    State = #state{pidmap = PidMap, mod = CBMod, modstate = CBState0}) ->
    case PidMap of
	#{Pid := Keys} ->
            CBState1  = CBMod:handle_death(Pid, Reason, CBState0),
            CBState2  = CBMod:handle_pid_remove(Pid, Keys, CBState1),
            NewPidMap = maps:remove(Pid, PidMap),
            NewState  = State#state{modstate = CBState2, pidmap = NewPidMap},
            {noreply, NewState};
        error ->
	    %% ignore 'EXIT' for unknown pid
	    %%  under some special circumstance we might get an 'EXIT'
	    %%  for the same pid multiple times
            {noreply, State}
    end;

handle_info(Info, State) ->
    case catch cb(handle_info, [Info], State) of
        {noreply, CBState} ->
            {noreply, cb_state(CBState, State)};
        {stop, Reason, CBState} ->
            {stop, Reason, cb_state(CBState, State)};
        _ ->
            {noreply, State}
    end.

terminate(Reason, State) ->
    cb(terminate, [Reason], State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------------------------------
%% -- cb module wrappers

handle_register(Pid, Key, OtherArgs, State) ->
    case cb(handle_register, [Pid, Key, OtherArgs], State) of
        {ok, DelKeys, CBState} ->
	    {ok, DelKeys, cb_state(CBState, State)};
	Other ->
	    Other
    end.

handle_unregister(Key, OtherArgs, State) ->
    {RegPids, CBState} = cb(handle_unregister, [Key, OtherArgs], State),
    {RegPids, cb_state(CBState, State)}.

handle_update(OldKey, NewKey, OtherArgs, State) ->
    {RegPids, CBState} =
	try cb(handle_update, [OldKey, NewKey, OtherArgs], State)
	catch
	    error:undef ->
		default_update(OldKey, NewKey, OtherArgs, State)
	end,
    {RegPids, cb_state(CBState, State)}.

handle_pid_remove(Pid, Keys, State) ->
    CBState = cb(handle_pid_remove, [Pid, Keys], State),
    cb_state(CBState, State).

%% ------------------------------------------------------------------------------------------
%% -- helpers

cb(F, A,  #state{mod = CBMod, modstate = CBState}) ->
    apply(CBMod, F, A ++ [CBState]).

cb_state(CBState, State) ->
    State#state{modstate = CBState}.

remove_pid_key(Pid, Key, PidMap) ->
    case PidMap of
	#{Pid := [Key]} ->
            unlink(Pid),
            maps:remove(Pid, PidMap);
        #{Pid := Keys} ->
            PidMap#{Pid => ordsets:del_element(Key, Keys)};
        _ ->
            PidMap
    end.

update_pid_key(Pid, OldKey, NewKey, PidMap) ->
	maps:update_with(Pid,
			 fun(Old) ->
				 ordsets:add_element(NewKey, ordsets:del_element(OldKey, Old))
			 end, NewKey, PidMap).

default_update(OldKey, NewKey, OtherArgs, #state{mod = CBMod, modstate = CBState}) ->
	{RegPids, CBState1} = CBMod:handle_unregister(OldKey, OtherArgs, CBState),
	NewCBState = lists:foldl(
		       fun(Pid, StateX) ->
			       {ok, _, NewStateX} =
				   CBMod:handle_register(Pid, NewKey, OtherArgs, StateX),
			       NewStateX
		       end, CBState1, RegPids),
	{RegPids, NewCBState}.
