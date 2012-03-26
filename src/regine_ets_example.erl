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

handle_unregister(Key, _Args, Table) ->
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
