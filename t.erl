-module(t).

-export([go/0, go/1, go/2, go/3]).

go() ->
    go(7).

go(NrOfProcesses) ->
    [go(G, NrOfProcesses) || G <- [500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,12500,15000,17500,20000,30000,40000,80000,100000]].

go(Keys, NrOfProcesses) ->
    io:format("=====CASE: ~p =============================~n", [Keys]),
    go(Keys, 1000*1000*4, NrOfProcesses).

go(Keys, N, NrOfProcesses) ->
    erlang:display(with),
    {Time1, ok} = timer:tc(fun()->go(Keys, N, true, NrOfProcesses)end),
    erlang:display(without),
    {Time2, ok} = timer:tc(fun()->go(Keys, N, false, NrOfProcesses)end),
    io:format("~f% slower than without  (time with: ~p time without ~p)~n",[(((Time1)/(Time2))-1)*100, Time1, Time2]).

go(Keys, N, WC, NrOfProcesses) ->
    T = ets:new(xxx,[public,{write_concurrency,true},{decentralized_counters, WC}]),
    [ loop_client(T, N, Keys, 1, self()) || _ <- lists:seq(1, NrOfProcesses) ],
    [receive done -> ok end || _ <- lists:seq(1, NrOfProcesses) ],
    %loop(T, N, Keys, 1),
    Stats = stats(T, N*2),
    ets:delete(T),
    erlang:display(Stats),
    erlang:display([S || S <- Stats, element(1,S) =:= grows orelse element(1,S) =:= shrinks]),
    io:format("~p~n",[lists:sum([element(2,S) || S <- Stats, element(1,S) =:= grows orelse element(1,S) =:= shrinks])]),
    GropwsAndShrinks = lists:sum([element(2,S) || S <- Stats, element(1,S) =:= grows orelse element(1,S) =:= shrinks]),
    erlang:display(Stats),
    io:format("Ops ~p:  \n", [N]),
    io:format("GrowsAndShrinksPerOp ~f~n",[GropwsAndShrinks/(N*2)]).

stats(T, Ops) ->
    Keys = ets:info(T, size),
    {Buckets, Load, _, _, Min, Max, _, Grow, Shrink} = ets:info(T, stats),
    [{keys, Keys},
     {buckets, Buckets},
     {load, Load},
     {min, Min},
     {max, Max},
     {grows, Grow, Grow / Ops},
     {shrinks, Shrink, Shrink / Ops}].

loop_client(T, N, Keys, I, Parent) ->
    spawn(fun() ->
                  loop(T, N, Keys, I),
                  Parent ! done
          end).

loop(_, 0, _, _) ->
    ok;
loop(T, N, Keys, I) ->
    Cr = rand:uniform(Keys),
    ets:insert(T, {Cr}),
    Del = rand:uniform(Keys),
    ets:delete(T, Del),

    %% case (I band (I-1)) of
    %%     0 ->
    %%         erlang:display(stats(T, I*2)),
    %%         io:format("~p: ~p\n", [I, ets:info(T,size)]);
    %%     _ ->
    %%         ok
    %% end,

    loop(T, N-1, Keys, I+1).
