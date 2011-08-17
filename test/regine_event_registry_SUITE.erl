-module(regine_event_registry_SUITE).
-compile(export_all).
-define(SERVER, regine_event_registry_test).

subscribe_for_event(_Config) ->
    regine_event_registry:start_link(?SERVER),
    Pid_1 = test_process(self(), event),
    regine_event_registry:subscribe(?SERVER, event, Pid_1),
    timer:sleep(50),
    [Pid_1] = regine_event_registry:get_subscribers(?SERVER, event),
    [event] = regine_event_registry:get_subscriptions(?SERVER, Pid_1),
    regine_event_registry:terminate([], []),
    true.

unsubscribe_for_event(_Config) ->
    [Pid_1] = regine_event_registry:get_subscribers(?SERVER, event),
    regine_event_registry:unsubscribe(?SERVER, Pid_1),
    [] = regine_event_registry:get_subscribers(?SERVER, event),
    [] = regine_event_registry:get_subscriptions(?SERVER, Pid_1),
    Pid_1 ! {dead, self()},
    receive
        {dead, Pid_1} -> ok
    after 
        100 -> exit(not_dead)
    end.







all() ->
    [subscribe_for_event, unsubscribe_for_event].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_process(Pid, EventWaiting) ->
    spawn(fun() ->
                receive
                    {'EVENT', ?SERVER, EventWaiting, EventData} ->
                        Pid ! {event_get, self(), EventData};
                    {dead, From} -> 
                        From ! {dead, self()}
                end
          end).

kill_test_process(Pid) ->
    Pid ! self(),
    receive
        {dead, Pid} -> ok
    after
        1000 -> error(timeout)
    end.
