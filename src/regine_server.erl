%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(regine_server).
-behaviour(gen_server).

-export([start/2, start/3, start_link/2, start_link/3, register/4, unregister/3,
         lookup_pid/2, behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------------------------------
%% -- API
behaviour_info(callbacks) ->
    %% TODO: code_change/3
    [{init,1}, {handle_register,4}, {handle_unregister,2}, {handle_death,3}, {terminate,2}];
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
    gen_server:call(Server, {unregister, Key, OtherArgs}).

lookup_pid(Server, Pid) when is_pid(Pid) ->
    gen_server:call(Server, {lookup_pid, Pid});
lookup_pid(_, _) ->
    error(badarg).

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
handle_call({unregister, Key, OtherArgs}, _From, State = #state{pidmap = PidMap, mod = CBMod, modstate = CBState}) ->
    {RegPids, NewCBState} = CBMod:handle_unregister(Key, CBState, OtherArgs),
    NewPidMap = lists:foldl(fun (Pid, Acc) -> unregister_pid(Key, Pid, Acc) end, PidMap, RegPids),
    NewState  = State#state{modstate = NewCBState, pidmap = NewPidMap},
    {reply, ok, NewState};
handle_call({lookup_pid, Pid}, _From, State = #state{pidmap = PidMap}) ->
    case dict:find(Pid, PidMap) of
        {ok, Keys} ->
            {reply, Keys, State};
        error ->
            {reply, [], State}
    end;
handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State = #state{pidmap = PidMap, mod = CBMod, modstate = CBState}) ->
    case dict:find(Pid, PidMap) of
        {ok, Keys} ->
            NewCBState = lists:foldl(fun (Key, Acc) -> CBMod:handle_death(Key, Pid, Acc) end, CBState, Keys),
            NewPidMap  = dict:erase(Pid, PidMap),
            NewState   = State#state{modstate = NewCBState, pidmap = NewPidMap},
            {noreply, NewState};
        error ->
            {stop, Reason, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{mod = CBMod, modstate = CBState}) ->
    CBMod:terminate(Reason, CBState).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------------------------------
%% -- helpers
unregister_pid(Key, Pid, PidMap) ->
    case dict:find(Pid, PidMap) of
        {ok, [Key]} ->
            unlink(Pid),
            dict:erase(Pid, PidMap);
        {ok, [_OtherKey]} ->
            PidMap;
        {ok, AllKeys} ->
            dict:store(Pid, ordsets:delete(Key, AllKeys), PidMap)
    end.
