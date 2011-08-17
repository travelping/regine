-module(regine_event_registry_SUITE).
-compile(export_all).
-define(SERVER, regine_event_registry_test).

subscribe_for_event(_Config) ->
    Pid_1 = test_process(self(), [event]),
    regine_event_registry:subscribe(?SERVER, event, Pid_1),
    timer:sleep(50),
    [Pid_1] = regine_event_registry:get_subscribers(?SERVER, event),
    [event] = regine_event_registry:get_subscriptions(?SERVER, Pid_1),
    regine_event_registry:terminate([], []),
    true.

unsubscribe_for_event(_Config) ->
    [Pid_1] = regine_event_registry:get_subscribers(?SERVER, event),
    regine_event_registry:unsubscribe(?SERVER, Pid_1),
    timer:sleep(200),
    [] = regine_event_registry:get_subscribers(?SERVER, event),
    [] = regine_event_registry:get_subscriptions(?SERVER, Pid_1),
    kill_test_process(Pid_1).

publish_1(_Config) ->
    Pid = test_process(self(), [event]),
    regine_event_registry:subscribe(?SERVER, event, Pid),
    regine_event_registry:publish(?SERVER, event, ed1),
    receive
        {event_get, Pid, ed1} -> ok
    after
        500 -> exit(event_not_exist)
    end,
    regine_event_registry:unsubscribe(?SERVER, Pid),
    kill_test_process(Pid).



publish_2(_Config) ->
    System = spawn(fun() -> test_system() end),
    Pid_2 = test_process(System, [event, event_2]),
    Pid_3 = test_process(System, [event]),
    System ! {self(), [{ed1, [Pid_2, Pid_3]}, {ed2, [Pid_2] }]},
    timer:sleep(100),
    regine_event_registry:subscribe(?SERVER, event, Pid_2),
    regine_event_registry:subscribe(?SERVER, event, Pid_3),
    regine_event_registry:subscribe(?SERVER, event_2, Pid_2),
    regine_event_registry:publish(?SERVER, event, ed1),
    regine_event_registry:publish(?SERVER, event_2, ed2),
    receive
        ok -> ok
    after
        1000 -> exit(not_receive_ok)
    end.



all() ->
    [subscribe_for_event, unsubscribe_for_event, publish_1, publish_2].

init_per_suite(Config) ->
    {ok, Pid} = regine_event_registry:start_link(?SERVER),
    Config.

end_per_suite(_Config) ->
    ok.

test_process(Pid, EventsWaiting) ->
    spawn(fun() -> test_proc(Pid, EventsWaiting) end).

test_proc(Pid, EventsWaiting) ->
    receive
        {'EVENT', ?SERVER, Event, EventData} ->
            case lists:member(Event, EventsWaiting) of
                true ->
                    Pid ! {event_get, self(), EventData},
                    test_proc(Pid, EventsWaiting);
                false ->
                    error(unwanted_event)
            end;
        From ->
            From ! {dead, self()}
    end.


kill_test_process(Pid) ->
    Pid ! self(),
    receive
        {dead, Pid} -> ok
    after
        1000 -> error(timeout)
    end.

test_system() ->
    receive
        {RulePid, Data} -> test_system(RulePid, Data)
    after 1000
        -> error(no_data)
    end.


test_system(RulePid, []) ->
    RulePid ! ok;

% Data = [{EventData, [Pid]}]
test_system(RulePid, Data) ->
    receive
        {event_get, Pid, EventData} ->
            AllPids = proplists:get_value(EventData, Data),
            NewData1 = proplists:delete(EventData, Data),
            case lists:delete(Pid, AllPids) of
                []      -> NewData2 = NewData1;
                NewPids -> NewData2 = [{EventData, NewPids} | NewData1 ]
            end,
            test_system(RulePid, NewData2)
    after
        1000 -> error(not_all)
    end.
