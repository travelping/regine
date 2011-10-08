%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>

-module(regine_ets_example).
-export([start/0, lookup/1, register/2, unregister/1]).

-behaviour(regine_server).
-export([init/1, handle_register/4, handle_unregister/3, handle_pid_remove/3, handle_death/3, terminate/2]).

-define(NAME, ?MODULE).

%% ------------------------------------------------------------------------------------------
%% -- API
start() ->
    regine_server:start({local, ?NAME}, ?MODULE, {}).

lookup(Key) ->
    ets:lookup(?NAME, Key).

register(Key, Pid) ->
    regine_server:register(?NAME, Pid, Key, undefined).

unregister(Key) ->
    regine_server:unregister(?NAME, Key, undefined).

%% ------------------------------------------------------------------------------------------
%% -- regine_server callbacks
init({}) ->
    Table = ets:new(?NAME, [bag, protected, named_table, {read_concurrency, true}]),
    {ok, Table}.

handle_register(Pid, Key, _Args, Table) ->
    ets:insert(Table, {Key, Pid}),
    {ok, [Key], Table}.

handle_unregister(Key, Table, _Args) ->
    Pids = ets:lookup(Table, Key),
    ets:delete(Key, Table),
    {Pids, Table}.

handle_pid_remove(Pid, Keys, Table) ->
    lists:foreach(fun (Key) ->
                          ets:delete_object(Table, {Key, Pid})
                  end, Keys),
    Table.

handle_death(_Pid, _Reason, Table) -> Table.
terminate(_Reason, _State) -> ok.
