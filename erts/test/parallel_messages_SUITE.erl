-module(parallel_messages_SUITE).

-export([all/0,
         test_throughput_benchmark/1]).

all() -> [test_throughput_benchmark].

get_op([{_,O}], _RandNum) ->
    O;
get_op([{Prob,O}|Rest], RandNum) ->
    case RandNum < Prob of
        true -> O;
        false -> get_op(Rest, RandNum)
    end.
do_op(ProbHelpTab, Operations, Receiver) ->
    RandNum = rand:uniform(),
    Op = get_op(ProbHelpTab, RandNum),
    TheOp = Operations(Op),
    TheOp(Receiver).
do_work(WorksDoneSoFar, ProbHelpTab, Operations, Receiver) ->
    receive
        stop -> WorksDoneSoFar
    after
        0 -> do_op(ProbHelpTab, Operations, Receiver),
             do_work(WorksDoneSoFar + 1, ProbHelpTab, Operations, Receiver)
    end.

-record(parallel_messages_bench_config,
        {benchmark_duration_ms = 500,
         recover_time_ms = 1000,
         thread_counts = not_set,
         nr_of_repeats = 3,
         report_receive_throughput = [false, true],
         scenarios =
             [
              [
               {1.0, {message_size, 1}}
              ],
              [
               {1.0, {message_size, 10}}
              ],
              [
               {1.0, {message_size, 100}}
              ],
              [
               {1.0, {message_size, 1000}}
              ],
              [
               {1.0, {message_size, 10000}}
              ]
             ],
         notify_res_fun = fun(_Name, _Throughput) -> ok end,
         print_result_paths_fun =
             fun(ResultPath, _LatestResultPath) ->
                     Comment =
                         io_lib:format("<a href=\"file:///~s\">Result visualization</a>",[ResultPath]),
                     {comment, Comment}
             end
       }).

stdout_notify_res(ResultPath, LatestResultPath) ->
    io:format("Result Location: /~s~n", [ResultPath]),
    io:format("Latest Result Location: ~s~n", [LatestResultPath]).

throughput_benchmark() ->
    throughput_benchmark(
      #parallel_messages_bench_config{}).

throughput_benchmark(
  #parallel_messages_bench_config{
     benchmark_duration_ms  = BenchmarkDurationMs,
     recover_time_ms        = RecoverTimeMs,
     thread_counts          = ThreadCountsOpt,
     nr_of_repeats          = NrOfRepeats,
     report_receive_throughput = ReportReceiveThroughputList,
     scenarios              = Scenarios,
     notify_res_fun         = NotifyResFun,
     print_result_paths_fun = PrintResultPathsFun}) ->
    NrOfSchedulers = erlang:system_info(schedulers),
    %Parent = self(),
    %% Mapping benchmark operation names to their action
    Operations =
        fun({message_size, Size}) ->
                case get(Size) of
                    undefined ->
                        Msg = lists:seq(1, Size),
                        NewSendFun =
                            fun(Receiver) ->
                                    Receiver ! Msg
                            end,
                        put(Size, NewSendFun),
                        NewSendFun;
                    SendFun ->
                        SendFun
                end
        end,
    %% Helper functions
    CalculateThreadCounts =
        fun Calculate([Count|Rest]) ->
                case Count > NrOfSchedulers of
                    true -> lists:reverse(Rest);
                    false -> Calculate([Count*2,Count|Rest])
                end
        end,
    CalculateOpsProbHelpTab =
        fun Calculate([{_, OpName}], _) ->
                [{1.0, OpName}];
            Calculate([{OpPropability, OpName}|Res], Current) ->
                NewCurrent = Current + OpPropability,
                [{NewCurrent, OpName}| Calculate(Res, NewCurrent)]
        end,
    RenderScenario =
        fun R([], StringSoFar) ->
                StringSoFar;
            R([{Fraction, Operation}], StringSoFar) ->
                io_lib:format("~s ~f% ~p",[StringSoFar, Fraction * 100.0, Operation]);
            R([{Fraction, Operation}|Rest], StringSoFar) ->
                R(Rest,
                  io_lib:format("~s ~f% ~p, ",[StringSoFar, Fraction * 100.0, Operation]))
        end,
    DataHolder =
        fun DataHolderFun(Data)->
                receive
                    {get_data, Pid} -> Pid ! {ets_bench_data, Data};
                    D -> DataHolderFun([Data,D])
                end
        end,
    DataHolderPid = spawn_link(fun()-> DataHolder([]) end),
    PrintData =
        fun (Str, List) ->
                io:format(Str, List),
                DataHolderPid ! io_lib:format(Str, List)
        end,
    GetData =
        fun () ->
                DataHolderPid ! {get_data, self()},
                receive {ets_bench_data, Data} -> Data end
        end,
    %% Function that runs a benchmark instance and returns the number
    %% of operations that were performed
    RunBenchmark =
        fun({NrOfProcs, Scenario, Duration}) ->
                ProbHelpTab = CalculateOpsProbHelpTab(Scenario, 0),
                ParentPid = self(),
                ReceiveFun =
                    fun ReceiveFun(NrOfStops, ReceiveCount) when NrOfStops =:= NrOfProcs ->
                            ParentPid ! {done_nothing_more_to_receive, ReceiveCount};
                        ReceiveFun(NrOfStops, ReceiveCount) ->
                            receive
                                Msg ->
                                    case Msg of
                                        stop ->
                                            ReceiveFun(NrOfStops + 1, ReceiveCount);
                                        _ ->
                                            ReceiveFun(NrOfStops, ReceiveCount + 1)
                                    end
                            end
                    end,
                Receiver = spawn_opt(fun() -> ReceiveFun(0, 0) end,
                                     [{message_queue_data, off_heap}]),
                Worker =
                    fun() ->
                            receive start -> ok end,
                            WorksDone =
                                do_work(0, ProbHelpTab, Operations, Receiver),
                            ParentPid ! WorksDone,
                            Receiver ! stop
                    end,
                ChildPids =
                    lists:map(fun(_N) -> spawn_link(Worker) end, lists:seq(1, NrOfProcs)),
                erlang:garbage_collect(),
                timer:sleep(RecoverTimeMs),
                lists:foreach(fun(Pid) -> Pid ! start end, ChildPids),
                timer:sleep(Duration),
                lists:foreach(fun(Pid) -> Pid ! stop end, ChildPids),
                TotalWorksDone = lists:foldl(
                                   fun(_, Sum) ->
                                           receive
                                               Count -> Sum + Count
                                           end
                                   end, 0, ChildPids),
                {TimeAfterSends, ok} =
                    timer:tc(
                      fun() ->
                              receive
                                  {done_nothing_more_to_receive, ReceiveCount} ->
                                      %% Sanity check
                                      ReceiveCount = TotalWorksDone,
                                      ok
                              end
                      end),
                {Duration + (TimeAfterSends div 1000), TotalWorksDone}
        end,
    RunBenchmarkInSepProcess =
        fun(ParameterTuple) ->
                P = self(),
                Results =
                    [begin
                         spawn_link(fun()-> P ! {bench_result, RunBenchmark(ParameterTuple)} end),
                         receive {bench_result, Res} -> Res end
                     end || _ <- lists:seq(1, NrOfRepeats)],
                {R1, R2} = lists:foldl(fun ({I1, I2}, {A1, A2}) ->
                                               {I1 + A1, I2 + A2}
                                       end, {0, 0}, Results),
                {R1 / NrOfRepeats, R2 / NrOfRepeats}
        end,
    RunBenchmarkAndReport =
        fun(ThreadCount,
            Scenario,
            Duration,
            ReportReceive) ->
                {ReceiveTime, NrOfSends} =
                    RunBenchmarkInSepProcess({ThreadCount,
                                              Scenario,
                                              Duration}),
                Throughput =
                    case ReportReceive of
                        true ->
                            NrOfSends/(ReceiveTime/1000.0);
                        false ->
                            NrOfSends/(Duration/1000.0)
                    end,
                PrintData("; ~f",[Throughput]),
                Name = io_lib:format("Scenario: ~w, "
                                     "# of Processes: ~w",
                                     [Scenario, ThreadCount]),
                NotifyResFun(Name, Throughput)
        end,
    ThreadCounts =
        case ThreadCountsOpt of
            not_set ->
                CalculateThreadCounts([1]);
            _ -> ThreadCountsOpt
        end,
    Version =
        (fun() ->
                 VersionString =  erlang:system_info(system_version),
                 case re:run(VersionString, "\\[(source\\-[^\\]]+)\\]") of
                     {match, [_, {StartPos, Length}]} ->
                         string:slice(VersionString, StartPos, Length);
                     _ ->
                         erlang:system_info(otp_release)
                 end
         end)(),
    %% Run the benchmark
    PrintData("# Each instance of the benchmark runs for ~w seconds:~n", [BenchmarkDurationMs/1000]),
    PrintData("# The result of a benchmark instance is presented as a number representing~n",[]),
    PrintData("# the number of operations performed per second:~n~n~n",[]),
    PrintData("# To plot graphs for the results below:~n",[]),
    PrintData("# 1. Open \"$ERL_TOP/lib/stdlib/test/ets_SUITE_data/visualize_throughput.html\" in a web browser~n",[]),
    PrintData("# 2. Copy the lines between \"#BENCHMARK STARTED$\" and \"#BENCHMARK ENDED$\" below~n",[]),
    PrintData("# 3. Paste the lines copied in step 2 to the text box in the browser window opened in~n",[]),
    PrintData("#    step 1 and press the Render button~n~n",[]),
    PrintData("#BENCHMARK STARTED$~n",[]),
    %% The following loop runs all benchmark scenarios and prints the results (i.e, operations/second)
    lists:foreach(
      fun(Scenario) ->
              lists:foreach(
                fun(ReportReceiveThroughput) ->
                        PrintData("Scenario: ~s, send_duration=~p ms, ~s$~n",
                                  [case ReportReceiveThroughput of
                                       true -> "Receive Throuput";
                                       false -> "Send Throuput"
                                   end,
                                   BenchmarkDurationMs,
                                   RenderScenario(Scenario, "")]),
                        lists:foreach(
                          fun(ThreadCount) ->
                                  PrintData("; ~w",[ThreadCount])
                          end,
                          ThreadCounts),
                        PrintData("$~n",[]),
                        PrintData(Version,[]),
                        lists:foreach(
                          fun(ThreadCount) ->
                                  RunBenchmarkAndReport(ThreadCount,
                                                        Scenario,
                                                        BenchmarkDurationMs,
                                                        ReportReceiveThroughput)
                          end,
                          ThreadCounts),
                        PrintData("$~n",[])
                end,
                ReportReceiveThroughputList)
      end,
      Scenarios),
    PrintData("~n#BENCHMARK ENDED$~n~n",[]),
    DataDir = filename:join(filename:dirname(code:which(?MODULE)), "parallel_messages_SUITE_data"),
    TemplatePath = filename:join(DataDir, "visualize_throughput.html"),
    {ok, Template} = file:read_file(TemplatePath),
    OutputData = string:replace(Template, "#bench_data_placeholder", GetData()),
    OutputPath1 = filename:join(DataDir, "ets_bench_result.html"),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:timestamp()),
    StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
    OutputPath2 = filename:join(DataDir, io_lib:format("ets_bench_result_~s.html", [StrTime])),
    file:write_file(OutputPath1, OutputData),
    file:write_file(OutputPath2, OutputData),
    PrintResultPathsFun(OutputPath2, OutputPath1).


test_throughput_benchmark(_) ->
    throughput_benchmark().
