-module(at_test).

-export([run_s/0, run_a/0]).
-import(at_server, [start/1, stop/1, doquery/2, begin_t/1, query_t/3, update_t/3, commit_t/2]).
-import(at_extapi, [abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).

run_s() -> 
    {_,AT} = at_server:start(start),
    {_,Ref1} = at_server:begin_t(AT),
    {_,Ref2} = at_server:begin_t(AT),
    {_,Ref3} = at_server:begin_t(AT),
    QueryFun =
        fun (Data) ->
            Data
        end,
    FailFun =
        fun (Data) ->
            Data + 12
        end,
    Old = at_server:doquery(AT, QueryFun),
    Ab2 = query_t(AT, Ref3, FailFun),
    query_t(AT, Ref1, QueryFun),
    UpdateFun = 
		fun (_) ->			
			updated
		end,    
    update_t(AT, Ref1, UpdateFun),
    query_t(AT, Ref1, QueryFun),
    commit_t(AT, Ref1),
    New = doquery(AT, QueryFun),
    Ab = query_t(AT, Ref2, QueryFun),
    stop(AT),
    {Old, New, Ab, Ab2}.
    
run_a() ->
    {_,AT} = start(10),
    Double = 
		fun (State) ->			
			State + State
		end,    
    tryUpdate(AT, Double),
    QueryFun =
        fun (Data) ->
            Data
        end,
    doquery(AT, QueryFun),
    {_,Ref1} = begin_t(AT),
    {_,Ref2} = begin_t(AT),
    abort(AT, Ref1),
    abort(AT, Ref2),
    TwoFun =
        fun (State, Element) ->
            State + Element
        end,
    choiceUpdate(AT, TwoFun, ['a', 'a', 3, 'c', 5, 6, 7, 8, 9]),
    doquery(AT, QueryFun).
    
    