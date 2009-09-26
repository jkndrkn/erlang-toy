-module(array_test).
-compile(export_all).

% Compares sequential and random access of Erlang native lists and arrays
%
% Usage: array_test:main(Magnitude,Iterations)
%
% Magnitude is the number of zeros in the size of the test arrays and lists.
% Iterations is the number of times the benchmarks are executed prior to averaging.

main(Magnitude) ->
    main(Magnitude, 10).

% TODO: Implement iterations feature.
main(Magnitude,_Iterations) -> 
    Size = round(math:pow(10, Magnitude)),
    Benchmarks = 
	[
	 {list_seq,fun(S) -> list_seq(S) end},
	 {list_rand,fun(S) -> list_rand(S) end},
	 {array_seq,fun(S) -> array_seq(S) end},
	 {array_rand,fun(S) -> array_rand(S) end}
	],
    benchmark_run(Size, Benchmarks).

benchmark_run(S,B) ->
    benchmark_run(S,B,[]).

benchmark_run(_,[],Results) -> Results;
benchmark_run(Size,[{BenchmarkName,Benchmark}|Benchmarks],Results) ->
    TimeStart = time_microseconds(),
    Benchmark(Size),
    TimeEnd = time_microseconds(),
    Result = {BenchmarkName,TimeEnd - TimeStart},
    benchmark_run(Size,Benchmarks,[Result|Results]).    

list_build(Size) ->
    list_build(Size, []).

list_build(0, List) -> List;
list_build(Size, List) ->
    list_build(Size - 1, [random:uniform() | List]).

list_seq(Size) ->
    List1 = list_build(Size),
    List2 = list_build(Size),
    list_seq(List1, List2, []).

list_seq([], [], List3) ->
    List3;
list_seq([H1|T1], [H2|T2], List3) ->
    list_seq(T1, T2, [H1 * H2 | List3]).

list_rand(Size) ->
    List1 = list_build(Size),
    List2 = list_build(Size),
    list_rand(Size,List1, List2, []).

list_rand(Size,List1,List2,List3) ->
    list_rand(Size,Size,List1,List2,List3).

list_rand(0,_,_,_,List3) ->
    List3;
list_rand(Size,Length,List1,List2,List3) ->
    Index1 = random:uniform(Length),
    Index2 = random:uniform(Length),
    Result = lists:nth(Index1, List1) * lists:nth(Index2, List2),
    list_rand(Size - 1, List1, List2, [Result | List3]).

array_build(Size) ->
    Array = array:new(Size),
    array_build(Size, Array).

array_build(0,Array) ->
    Array;
array_build(Size,Array) ->
    array_build(Size - 1, array:set(Size - 1, random:uniform(), Array)).   

array_seq(Size) ->
    Array1 = array_build(Size),
    Array2 = array_build(Size),
    array_seq(Size, Array1, Array2).

array_seq(Size,Array1,Array2) ->
    array_seq(Size, Array1, Array2, array:new(Size)).

array_seq(0,_,_,Array3) ->
    Array3;
array_seq(Size, Array1, Array2, Array3) ->
    Index = Size - 1,
    Result = array:get(Index, Array1) * array:get(Index, Array2),
    array_seq(Index, Array1, Array2, array:set(Index, Result, Array3)).    

array_rand(Size) ->
    Array1 = array_build(Size),
    Array2 = array_build(Size),
    array_rand(Size, Array1, Array2).

array_rand(Size,Array1,Array2) ->
    array_rand(Size,Size,Array1,Array2,array:new(Size)).

array_rand(0,_,_,_,Array3) ->
    Array3;
array_rand(Size,Length,Array1,Array2,Array3) ->
    Index1 = random:uniform(Length) - 1,
    Index2 = random:uniform(Length) - 1,
    Result = array:get(Index1, Array1) * array:get(Index2, Array2),
    array_rand(Size - 1, Length, Array1, Array2, array:set(Size - 1, Result, Array3)).

time_microseconds() ->
    {MS, S, US} = now(),
    (MS * 1.0e+12) + (S * 1.0e+6) + US.
