-module(load_test).

%%  CREATE TABLE IF NOT EXISTS `users`
%%      (`id` BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
%%       `name` VARCHAR(100) NOT NULL, PRIMARY KEY (`id`)) ENGINE = InnoDB

-define(QUERY, <<"SELECT id FROM users WHERE id = ?">>).

-export([
    bench/2,
    bench/4
]).

bench(Number, Concurrency) ->
    bench(Number, Concurrency, pool, false).

bench(Number, Concurrency, Mode, Profile) ->
    case mysql_pool:start() of
        ok ->
            mysql_pool:prepare(mypool, get_accounts, ?QUERY);
        _ ->
            ok
    end,

    Self = self(),
    LoopNumbers = Number div Concurrency,

    {List, BenchFun} = case Mode of
        pool ->
            ExecFun = fun() -> {ok, _, _} = mysql_pool:execute(mypool, get_accounts, [1]) end,
            PList = lists:seq(1, Concurrency),
            PFun = fun(_) -> loop(LoopNumbers, ExecFun) end,
            {PList, PFun};
        connection ->
            ConnectionOptions = bench_connection_options(),
            CList0 = lists:seq(1, Concurrency),
            CList = lists:map(fun(_) -> bench_connection(ConnectionOptions) end, CList0),
            CFun = fun(Pid) -> bench_connection_loop(LoopNumbers, Pid) end,
            {CList, CFun}
    end,

    profilers_start(Profile),
    A = os:timestamp(),
    Pids = [spawn_link(fun() -> BenchFun(X), Self ! {self(), done} end) || X <- List],
    [receive {Pid, done} -> ok end || Pid <- Pids],
    B = os:timestamp(),
    profiler_stop(Profile),

    print(Number, A, B).

bench_connection(ConnectionOptions) ->
    {ok, Pid} = mysql_connection:start_link(ConnectionOptions),
    {ok, _} = mysql_connection:prepare(Pid, get_accounts, ?QUERY),
    Pid.

bench_connection_options() ->
    {ok, Pools} = mysql_utils:env(pools),
    MyPool = mysql_utils:lookup(mypool, Pools),
    mysql_utils:lookup(connection_options, MyPool).

bench_connection_loop(0, _ConnectionPid) ->
    ok;
bench_connection_loop(Nr, ConnectionPid) ->
    {ok, _, _} = mysql_connection:execute(ConnectionPid, get_accounts, [1]),
    bench_connection_loop(Nr-1, ConnectionPid).

print(Num, A, B) ->
    Microsecs = timer:now_diff(B, A),
    Time = Microsecs div Num,
    PerSec = case Time of
         0 ->
             "N/A";
         _ ->
             1000000 div Time
    end,

    io:format("### ~p ms ~p req/sec ~n", [Microsecs div 1000, PerSec]).

loop(0, _Fun) ->
    ok;
loop(Nr, Fun) ->
    Fun(),
    loop(Nr-1, Fun).

profilers_start(true)->
    {ok, P} = eprof:start(),
    eprof:start_profiling(processes() -- [P]);
profilers_start(_)->
    ok.

profiler_stop(true)->
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop();
profiler_stop(_)->
    ok.
