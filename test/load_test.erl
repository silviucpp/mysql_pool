-module(load_test).

%%  CREATE TABLE IF NOT EXISTS `users`
%%      (`id` BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
%%       `name` VARCHAR(100) NOT NULL, PRIMARY KEY (`id`)) ENGINE = InnoDB

-export([
    bench/2
]).

bench(Number, Concurrency) ->
    case mysql_pool:start() of
        ok ->
            mysql_pool:prepare(mypool, get_accounts, <<"SELECT id, name FROM users WHERE id = ?">>);
        _ ->
            ok
    end,

    Self = self(),
    List = lists:seq(1, Concurrency),
    LoopNumbers = Number div Concurrency,

    Fun = fun() -> {ok, _, _} = mysql_pool:execute(mypool, get_accounts, [1]) end,

    A = os:timestamp(),
    Pids = [spawn_link(fun() -> loop(LoopNumbers, Fun), Self ! {self(), done} end) || _ <- List],
    [receive {Pid, done} -> ok end || Pid <- Pids],
    B = os:timestamp(),

    print(Number, A, B).

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
