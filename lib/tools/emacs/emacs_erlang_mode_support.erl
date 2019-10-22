-module(emacs_erlang_mode_support).

-export([main/1]).


erlang_project_dir(ErlangFilePath) ->
    DirPath = filename:dirname(ErlangFilePath),
    Finder =
        fun FinderFun([], _) -> false;
            FinderFun(Path, Predicate)->
                case Predicate(Path) of
                    true -> Path;
                    false ->
                        FinderFun(filename:join(lists:droplast(filename:split(Path))), Predicate)
                end
        end,
    IsGit = fun(Path) -> filelib:is_dir(filename:join(Path, ".git"))  end,
    Finder(DirPath, IsGit).

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
    list_functions_in_module(ModuleNameStr);
main(["get_project_dir", FileNameStr]) ->
    io:format(erlang_project_dir(FileNameStr)).
