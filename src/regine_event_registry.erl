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

-module(regine_event_registry).
-export([start_link/1, publish/3, send_event/4, subscribe/3, get_subscribers/2, get_subscriptions/2, unsubscribe/2, unsubscribe/3]).

-behaviour(regine_server).
-export([init/1, handle_register/4, handle_unregister/3, handle_pid_remove/3, handle_death/3, terminate/2]).

%% ------------------------------------------------------------------------------------------
%% -- API
start_link(ServerName) when is_atom(ServerName) ->
    regine_server:start({local, ServerName}, ?MODULE, {ServerName}).

publish(ServerName, EventType, EventData) ->
    lists:foreach(fun ({_, Pid}) ->
                          Pid ! {'EVENT', ServerName, EventType, EventData}
                  end, ets:lookup(ServerName, EventType)).

send_event(ServerName, Pid, EventType, EventData) ->
    Pid ! {'EVENT', ServerName, EventType, EventData}.

subscribe(ServerName, EventType, Pid) ->
    regine_server:register(ServerName, Pid, EventType, undefined).

get_subscribers(ServerName, EventType) ->
    [Pid || {_, Pid} <- ets:lookup(ServerName, EventType)].

get_subscriptions(ServerName, Pid) ->
    regine_server:lookup_pid(ServerName, Pid).

unsubscribe(ServerName, Pid) ->
    regine_server:unregister_pid(ServerName, Pid).

unsubscribe(ServerName, Pid, EventType) ->
    regine_server:unregister_pid(ServerName, Pid, EventType).

%% ------------------------------------------------------------------------------------------
%% -- regine_server callbacks
init({ServerName}) ->
    Table = ets:new(ServerName, [bag, protected, named_table, {read_concurrency, true}]),
    {ok, Table}.

handle_register(Pid, EventType, _Args, Table) ->
    ets:insert(Table, {EventType, Pid}),
    {ok, [EventType], Table}.

handle_unregister(EventType, _Args, Table) ->
    Pids = ets:lookup(Table, EventType),
    ets:delete(EventType, Table),
    {Pids, Table}.

handle_pid_remove(Pid, PidEventTypes, Table) ->
    lists:foreach(fun (EventType) ->
                          ets:delete_object(Table, {EventType, Pid})
                  end, PidEventTypes),
    Table.

handle_death(_Pid, _Reason, Table) -> Table.
terminate(_Reason, _State)         -> ok.
