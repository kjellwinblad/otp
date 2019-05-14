-module(t).

-export([go/0, go/1, go/2, go/3]).


go() ->
    go(256).

go(Keys) ->
    go(Keys, 1000*1000).

go(Keys, N) ->
    go(Keys, N, true).

go(Keys, N, WC) ->
    T = ets:new(xxx,[public,{write_concurrency,WC}]),
    loop(T, N, Keys, 1),
    Stats = stats(T, N),
    ets:delete(T),
    Stats.

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



loop(_, 0, _, _) ->
    ok;
loop(T, N, Keys, I) ->
    Cr = rand:uniform(Keys),
    ets:insert(T, {Cr}),
    Del = rand:uniform(Keys),
    ets:delete(T, Del),

    case (I band (I-1)) of
        0 ->
            io:format("~p: ~p\n", [I, ets:info(T,size)]);
        _ ->
            ok
    end,

    loop(T, N-1, Keys, I+1).

    


