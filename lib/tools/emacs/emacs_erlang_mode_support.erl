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
                 fun(Path) -> filelib:is_file(filename:join(Path, "mix.exs"))  end,
                 fun(Path) -> filelib:is_file(filename:join(Path, "erlang.mk"))  end]] ++
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

erlang_project_lib_dirs(ProjectDir) ->
    BeamFiles = filelib:wildcard(filename:join(ProjectDir, "**/*.beam")),
    BeamDirs = [filename:dirname(BeamFile) || BeamFile <- BeamFiles],
    sets:to_list(sets:from_list(BeamDirs)).

erlang_project_libs_parameter(ProjectDir) ->
    LibDirs = erlang_project_lib_dirs(ProjectDir),
    io_lib:format("-pa ~s", [lists:join(" ", LibDirs)]).


erlang_project_add_project_libs_dirs_to_path(ProjectDir) ->
    LibDirs = erlang_project_lib_dirs(ProjectDir),
    code:add_paths(LibDirs).

erlang_project_add_project_libs_dirs_to_path_from_cache(CacheDir, ProjectDir) ->
    LibsCacheFile = erlang_project_lib_dirs_cache_file(CacheDir, ProjectDir),
    case filelib:is_file(LibsCacheFile) of
        true ->
            {ok, [LibDirs]} = file:consult(LibsCacheFile),
            code:add_paths(LibDirs);
        false ->
            os:cmd(io_lib:format("escript ~s update_module_names_cache ~s ~s &", [escript:script_name(), CacheDir, filename:join(ProjectDir, "nofile")]))
    end.

erlang_project_all_modules(ProjectDir) ->
    erlang_project_add_project_libs_dirs_to_path(ProjectDir),
    [filename:rootname(filename:basename(F))
     || P <- code:get_path(), F <- filelib:wildcard(P ++ "/*.beam")].

erlang_project_cache_dir(CacheDir, ProjectDir) ->
    ProjectDirNames = tl(filename:split(ProjectDir)),
    filename:join(filename:split(CacheDir) ++ ProjectDirNames).

erlang_project_all_modules_cache_file(CacheDir, ProjectDir) ->
    ProjectCacheDir = erlang_project_cache_dir(CacheDir, ProjectDir),
    filename:join(ProjectCacheDir, "all_modules.txt").

erlang_project_libs_parameter_cache_file(CacheDir, ProjectDir) ->
    ProjectCacheDir = erlang_project_cache_dir(CacheDir, ProjectDir),
    filename:join(ProjectCacheDir, "erl_lib_param.txt").

erlang_project_lib_dirs_cache_file(CacheDir, ProjectDir) ->
    ProjectCacheDir = erlang_project_cache_dir(CacheDir, ProjectDir),
    filename:join(ProjectCacheDir, "erl_lib_dirs.txt").

erlang_project_update_all_modules_cache(CacheDir, ProjectDir) ->
    AllModulesFile = erlang_project_all_modules_cache_file(CacheDir, ProjectDir),
    filelib:ensure_dir(AllModulesFile),
    file:write_file(AllModulesFile, lists:join(";", erlang_project_all_modules(ProjectDir))),
    LibsParamCacheFile = erlang_project_libs_parameter_cache_file(CacheDir, ProjectDir),
    file:write_file(LibsParamCacheFile, erlang_project_libs_parameter(ProjectDir)),
    LibDrisCacheFile = erlang_project_lib_dirs_cache_file(CacheDir, ProjectDir),
    file:write_file(LibDrisCacheFile, io_lib:format("~tp.~n", [erlang_project_lib_dirs(ProjectDir)])).


erlang_project_all_modules_string(CacheDir, FileNameStr) ->
    ProjectDir = erlang_project_dir(FileNameStr),
    AllModulesFile = erlang_project_all_modules_cache_file(CacheDir, ProjectDir),
    case filelib:is_file(AllModulesFile) of
        true ->
            {ok, BinStr} =file:read_file(AllModulesFile),
            BinStr;
        false ->
            os:cmd(io_lib:format("escript ~s update_module_names_cache ~s ~s &", [escript:script_name(), CacheDir, FileNameStr])),
            lists:join(";", [atom_to_list(Module) || {Module, _Path} <- code:all_loaded()])
    end.

mk_empty_parameter_list_string(0) ->
    "()";
mk_empty_parameter_list_string(1) ->
    "(Arg)";
mk_empty_parameter_list_string(Arity) ->
    lists:flatten("(" ++ lists:join(", ", [io_lib:format("A~p", [A]) || A <- lists:seq(1, Arity)]) ++ ")").

list_functions_in_module(CacheDir, FileNameStr, ModuleNameStr) ->
    erlang_project_add_project_libs_dirs_to_path_from_cache(CacheDir, erlang_project_dir(FileNameStr)),
    ModuleName = list_to_atom(ModuleNameStr),
    lists:foreach(fun({FunName, Arity}) ->
                          io:format("~p:~p~s;", [ModuleName,
                                                 FunName,
                                                 mk_empty_parameter_list_string(Arity)])
                  end,
                  proplists:get_value(exports, ModuleName:module_info())).

main(["list_functions_in_module", CacheDir, FileNameStr, ModuleNameStr]) ->
    list_functions_in_module(CacheDir, FileNameStr, ModuleNameStr);
main(["get_project_dir", FileNameStr]) ->
    io:format(erlang_project_dir(FileNameStr));
main(["update_etags", FileNameStr]) ->
    update_etags(erlang_project_dir(FileNameStr));
main(["update_module_names_cache", CacheDir, FileNameStr]) ->
    erlang_project_update_all_modules_cache(CacheDir, erlang_project_dir(FileNameStr));
main(["list_modules", CacheDir, FileNameStr]) ->
    io:format(erlang_project_all_modules_string(CacheDir, FileNameStr)).
