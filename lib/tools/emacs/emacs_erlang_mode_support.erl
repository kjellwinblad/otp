-module(emacs_erlang_mode_support).

-export([main/1]).


%% erlang_project_dir(ErlangFilePath) ->
%%     Path = filename:dirname(ErlangFilePath),
%%     Finder =
%%         fun FinderFun(Path, Predicate) ->
%%                 case Predicate(Path) of
%%                     true -> Path;
%%                     false -> no
%%                 end,
%%                 ReversePathList = lists:reverse(filename:split(Path)),
%%                 lists:tail(ReversePathList),
%%                 lists:takewhile(fun() -> not Predicate(),)
%%                 ok
%%         end,
%%     ok.

mk_empty_parameter_list_string(0) ->
    "()";
mk_empty_parameter_list_string(1) ->
    "( )";
mk_empty_parameter_list_string(Arity) ->
    lists:flatten("(" ++ lists:duplicate(Arity -1, ", ") ++ ")").

list_functions_in_module(ModuleNameStr) ->
    ModuleName = list_to_atom(ModuleNameStr),
    lists:foreach(fun({FunName, Arity}) ->
                          io:format("~p:~p~s;", [ModuleName,
                                                 FunName,
                                                 mk_empty_parameter_list_string(Arity)])
                  end,
                  proplists:get_value(exports, ModuleName:module_info())).

main(["list_functions_in_module", ModuleNameStr]) ->
    list_functions_in_module(ModuleNameStr).
