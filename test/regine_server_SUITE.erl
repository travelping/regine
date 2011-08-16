-module(regine_server_SUITE).
-compile(export_all).

unregister_on_EXIT(_Config) ->
    TestPid = test_process(),
    TestKey = make_ref(),

    regine_ets_example:start(),
    regine_ets_example:register(TestKey, TestPid),
    [{TestKey, TestPid}] = regine_ets_example:lookup(TestKey),

    kill_test_process(TestPid),
    timer:sleep(100),
    [] = regine_ets_example:lookup(TestKey).

all() ->
    [unregister_on_EXIT].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


test_process() ->
    spawn(fun() -> receive From -> From ! {dead, self()} end end).

kill_test_process(Pid) ->
    Pid ! self(),
    receive
        {dead, Pid} -> ok
    after
        1000 -> error(timeout)
    end.
