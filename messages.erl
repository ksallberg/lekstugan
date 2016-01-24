-module(messages).
-export([init/0, run/1, killer/1]).

init() ->
    io:format("spin up~n"),
    P1 = spawn(?MODULE, run, [p2]),
    register(p1, P1),
    P2 = spawn(?MODULE, run, [p1]),
    register(p2, P2),
    p1 ! start,
    spawn(?MODULE, killer, [[p1, p2]]),
    messages_helper:help_me().

killer(KillList) ->
    io:format("time to kill..~n"),
    timer:sleep(25),
    lists:foreach(fun(Proc) ->
                      Proc ! stop,
                      unregister(Proc)
                  end,
                  KillList).

run(Partner) ->
    receive
        {val, Num} ->
            put(myval, Num),
            try
                Partner ! {val, Num+1},
                run(Partner)
            catch
                _:_ ->
                    StopNum = get(myval),
                    io:format("Opponent is dead, lets stop... ~p ~n",
                              [StopNum])
            end;
        start ->
            Partner ! {val, 0},
            run(Partner);
        stop ->
            StopNum = get(myval),
            io:format("I'm stopping (~p) final value: ~B~n", [self(), StopNum]),
            ok
    end.
