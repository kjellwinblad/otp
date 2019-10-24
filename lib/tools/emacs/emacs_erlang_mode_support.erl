-module(emacs_erlang_mode_support).

-export([main/1]).


erlang_project_dir(ErlangFilePath) ->
    DirPath = filename:dirname(ErlangFilePath),
    SplittedDirPath = filename:split(DirPath),
    FinderDown =
        fun FinderDownFun([], _) -> false;
            FinderDownFun(Path, Predicate)->
                case Predicate(Path) of
                    true -> Path;
                    false ->
                        case lists:droplast(filename:split(Path)) =:= [] of
                            true -> false;
                            false -> FinderDownFun(filename:join(lists:droplast(filename:split(Path))), Predicate)
                        end
                end
        end,
    FinderUp =
        fun FinderUpFun(PathList, [], Predicate) ->
                case Predicate(filename:join(PathList)) of
                    true -> filename:join(PathList);
                    false -> false
                end;
            FinderUpFun([], [ToAdd | Rest], Predicate) ->
                FinderUpFun([ToAdd], Rest, Predicate);
            FinderUpFun(PathListToCheck, [ToAdd | Rest], Predicate) ->
                case Predicate(filename:join(PathListToCheck)) of
                    true -> filename:join(PathListToCheck);
                    false ->
                        FinderUpFun(PathListToCheck ++ [ToAdd], Rest, Predicate)
                end
        end,
    Options =
        [FinderUp([], SplittedDirPath, Predicate) ||
            Predicate <-
                [fun(Path) -> filelib:is_file(filename:join(Path, ".emacs_erlang_mode_project"))  end,
                 fun(Path) -> filelib:is_file(filename:join(Path, "rebar.config"))  end,
                 fun(Path) -> filelib:is_file(filename:join(Path, "mix.exs"))  end]] ++
        [FinderDown(DirPath, Predicate) ||
            Predicate <-
                [fun(Path) -> filelib:is_dir(filename:join(Path, ".git"))  end,
                 fun(Path) -> filelib:is_dir(filename:join(Path, ".svn"))  end,
                 fun(Path) -> filelib:is_dir(filename:join(Path, ".hg"))  end,
                 fun(Path) -> filelib:is_dir(filename:join(Path, ".cvs"))  end,
                 fun(_) -> true end]],
    {value, Path} = lists:search(fun(V) -> V =/= false end, Options),
    Path.

update_etags(ProjectDir) ->
    ErlHrlFiles = filelib:wildcard(filename:join(ProjectDir, "**/*.{erl,hrl}")),
    os:cmd(io_lib:format("etags ~s", [lists:join(" ", ErlHrlFiles)])).

mk_empty_parameter_list_string(0) ->
    "()";
mk_empty_parameter_list_string(1) ->
    "(Arg)";
mk_empty_parameter_list_string(Arity) ->
    lists:flatten("(" ++ lists:join(", ", [io_lib:format("A~p", [A]) || A <- lists:seq(1, Arity)]) ++ ")").

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
    io:format(erlang_project_dir(FileNameStr));
main(["update_etags", FileNameStr]) ->
    update_etags(erlang_project_dir(FileNameStr)).
