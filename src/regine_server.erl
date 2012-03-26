%% Copyright 2011-2012, Travelping GmbH <info@travelping.com>

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
         unregister_pid/2, unregister_pid/3, lookup_pid/2, behaviour_info/1,
         call/2, call/3, cast/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------------------------------
%% -- API
behaviour_info(callbacks) ->
    %% TODO: code_change/3
    [{init,1}, {handle_register,4}, {handle_unregister,3}, {handle_pid_remove,3}, {handle_death,3}, {terminate,2}];
    %% these are optional: {handle_call, 3}, {handle_cast, 2}, {handle_info, 2}, {handle_update,4}
behaviour_info(_) ->
    undefined.

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
    
%% ------------------------------------------------------------------------------------------
%% -- gen_server callbacks
-record(state, {
    mod      :: module(),
    modstate :: term(),
    pidmap   :: dict()
}).

init({CBMod, CBArgs}) ->
    process_flag(trap_exit, true),

    case CBMod:init(CBArgs) of
        {ok, CBState} ->
            {ok, #state{pidmap = dict:new(), mod = CBMod, modstate = CBState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call({register, Pid, Key, OtherArgs}, _From, State = #state{mod = CBMod, modstate = CBState, pidmap = PidMap}) ->
    case CBMod:handle_register(Pid, Key, OtherArgs, CBState) of
        {ok, DelKeys, NewCBState} ->
            link(Pid), %% TODO: handle case when registered pid is not alive
            DKSet     = ordsets:from_list(DelKeys),
            NewPidMap = dict:update(Pid, fun (Old) -> ordsets:union(DKSet, Old) end, DelKeys, PidMap),
            NewState  = State#state{modstate = NewCBState, pidmap = NewPidMap},
            {reply, ok, NewState};
        ErrorReply = {error, _} ->
            {reply, ErrorReply, State}
    end;

handle_call({unregister_key, Key, OtherArgs}, _From, State = #state{pidmap = PidMap, mod = CBMod, modstate = CBState}) ->
    {RegPids, NewCBState} = CBMod:handle_unregister(Key, OtherArgs, CBState),
    NewPidMap = lists:foldl(fun (Pid, Acc) -> remove_pid_key(Pid, Key, Acc) end, PidMap, RegPids),
    NewState  = State#state{modstate = NewCBState, pidmap = NewPidMap},
    {reply, ok, NewState};

handle_call({update_key, OldKey, NewKey, OtherArgs}, _From, State = #state{pidmap = PidMap, mod = CBMod, modstate = CBState}) ->
	{RegPids, NewCBState} = try CBMod:handle_update(OldKey, NewKey, OtherArgs, CBState)
							catch
								error:undef ->
									default_update(OldKey, NewKey, OtherArgs, CBMod, CBState)
							end,
    NewPidMap = lists:foldl(fun (Pid, Acc) -> update_pid_key(Pid, OldKey, NewKey, Acc) end, PidMap, RegPids),
    NewState  = State#state{modstate = NewCBState, pidmap = NewPidMap},
    {reply, ok, NewState};

handle_call({unregister_pid, Pid}, _From, State = #state{pidmap = PidMap, mod = CBMod, modstate = CBState}) ->
    case dict:find(Pid, PidMap) of
        {ok, Keys} ->
            unlink(Pid),
            NewCBState = CBMod:handle_pid_remove(Pid, Keys, CBState),
            NewPidMap  = dict:erase(Pid, PidMap),
            NewState   = State#state{modstate = NewCBState, pidmap = NewPidMap},
            {reply, ok, NewState};
        error ->
            {reply, {error, unknown_pid}, State}
    end;

handle_call({unregister_pid_key, Pid, Key}, _From, State = #state{pidmap = PidMap, mod = CBMod, modstate = CBState}) ->
    case dict:is_key(Pid, PidMap) of
        true ->
            NewCBState = CBMod:handle_pid_remove(Pid, [Key], CBState),
            NewPidMap  = remove_pid_key(Pid, Key, PidMap),
            NewState   = State#state{modstate = NewCBState, pidmap = NewPidMap},
            {reply, ok, NewState};
        false ->
            {reply, {error, unknown_pid}, State}
    end;

handle_call({lookup_pid, Pid}, _From, State = #state{pidmap = PidMap}) ->
    case dict:find(Pid, PidMap) of
        {ok, Keys} ->
            {reply, Keys, State};
        error ->
            {reply, [], State}
    end;

handle_call({cb, Call}, From, State = #state{mod = CBMod, modstate = CBState0}) ->
    case catch CBMod:handle_call(Call, From, CBState0) of
        {reply, Reply, CBState1} ->
            {reply, Reply, State#state{modstate = CBState1}};
        {stop, Reason, Reply, CBState1} ->
            {stop, Reason, Reply, State#state{modstate = CBState1}};
        {stop, Reason, CBState1} ->
            {stop, Reason, State#state{modstate = CBState1}};
        Other ->
            {reply, Other, State}
    end;

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast({cb, Msg}, State = #state{mod = CBMod, modstate = CBState0}) ->
    case catch CBMod:handle_cast(Msg, CBState0) of
        {noreply, CBState1} ->
            {noreply, State#state{modstate = CBState1}};
        {stop, Reason, CBState1} ->
            {stop, Reason, State#state{modstate = CBState1}};
        _ ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State = #state{pidmap = PidMap, mod = CBMod, modstate = CBState0}) ->
    case dict:find(Pid, PidMap) of
        {ok, Keys} ->
            CBState1  = CBMod:handle_death(Pid, Reason, CBState0),
            CBState2  = CBMod:handle_pid_remove(Pid, Keys, CBState1),
            NewPidMap = dict:erase(Pid, PidMap),
            NewState  = State#state{modstate = CBState2, pidmap = NewPidMap},
            {noreply, NewState};
        error ->
			%% ignore 'EXIT' for unknown pid
			%%  under some special circumstance we might get an 'EXIT'
			%%  for the same pid multiple times
            {noreply, State}
    end;

handle_info(Info, State = #state{mod = CBMod, modstate = CBState0}) ->
    case catch CBMod:handle_info(Info, CBState0) of
        {noreply, CBState1} ->
            {noreply, State#state{modstate = CBState1}};
        {stop, Reason, CBState1} ->
            {stop, Reason, State#state{modstate = CBState1}};
        _ ->
            {noreply, State}
    end.

terminate(Reason, #state{mod = CBMod, modstate = CBState}) ->
    CBMod:terminate(Reason, CBState).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------------------------------
%% -- helpers
remove_pid_key(Pid, Key, PidMap) ->
    case dict:fetch(Pid, PidMap) of
        [Key] ->
            unlink(Pid),
            dict:erase(Pid, PidMap);
        [_OtherKey] ->
            PidMap;
        AllKeys ->
            dict:store(Pid, ordsets:del_element(Key, AllKeys), PidMap)
    end.

update_pid_key(Pid, OldKey, NewKey, PidMap) ->
	dict:update(Pid, fun(Old) -> ordsets:add_element(NewKey, ordsets:del_element(OldKey, Old)) end, NewKey, PidMap).

default_update(OldKey, NewKey, OtherArgs, CBMod, CBState) ->
	{RegPids, CBState1} = CBMod:handle_unregister(OldKey, OtherArgs, CBState),
	NewCBState = lists:foldl(fun(Pid, StateX) -> {ok, _, NewStateX} = CBMod:handle_register(Pid, NewKey, OtherArgs, StateX), NewStateX end, CBState1, RegPids),
	{RegPids, NewCBState}.
