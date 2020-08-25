-module(test).

-export([main/1]).

main(_List) ->
    T = ets:new(a, [ordered_set, {write_concurrency, true}, {seq_lock, true}]),
    ets:lookup(T, item),
    ets:insert(T, {item}),
    ets:lookup(T, item),
    ets:insert(T, {item}),
    ets:lookup(T, item),
    ok.
