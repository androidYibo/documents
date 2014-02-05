%%%-------------------------------------------------------------------
%%% @doc
%%% Atomic Transaction Server Tests.
%%% @end
%%%-------------------------------------------------------------------
%%% Student name: Niels G. W. Serup
%%%-------------------------------------------------------------------

-module(at_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test that basic function application works on the server.
function_application_test() ->
    A = random:uniform(4442),
    B = random:uniform(3342),
    {ok, AT} = at_server:start(A),
    M = A + B,
    {ok, M} = at_server:doquery(AT, fun(N) -> N + B end),
    error = at_server:doquery(AT, fun(_) -> throw(noway) end),
    {ok, A} = at_server:stop(AT).


%% Test that sequential transactions work.
sequential_transactions_test() ->
    {ok, AT} = at_server:start([3, 1]),
    
    {ok, Ref0} = at_server:begin_t(AT),
    {ok, [1]} = at_server:query_t(AT, Ref0, fun([_ | Xs]) -> Xs end),
    ok = at_server:update_t(AT, Ref0, fun(Xs) -> [ahem | Xs] end),
    {ok, [ahem, 3, 1]} = at_server:query_t(AT, Ref0, fun at_misc:id/1),
    {ok, [3, 1]} = at_server:doquery(AT, fun at_misc:id/1),
    ok = at_server:commit_t(AT, Ref0),
    aborted = at_server:query_t(Ref0, Ref0, fun at_misc:id/1),
    {ok, [ahem, 3, 1]} = at_server:doquery(AT, fun at_misc:id/1),
    aborted = at_server:query_t(AT, Ref0, fun at_misc:id/1),
    aborted = at_server:commit_t(AT, Ref0),
    aborted = at_server:update_t(AT, Ref0, fun at_misc:id/1),

    {ok, Ref1} = at_server:begin_t(AT),
    {ok, [ahem, 3, 1]} = at_server:query_t(AT, Ref1, fun at_misc:id/1),
    ok = at_server:update_t(AT, Ref1, fun(Xs) -> [ahem | Xs] end),
    {ok, [ahem, ahem, 3, 1]} = at_server:query_t(AT, Ref1, fun at_misc:id/1),
    {ok, [ahem, 3, 1]} = at_server:doquery(AT, fun at_misc:id/1),
    ok = at_server:update_t(AT, Ref1, fun(_) -> muuuuh end),
    ok = at_server:commit_t(AT, Ref1),
    aborted = at_server:commit_t(AT, Ref1),
    {ok, muuuuh} = at_server:doquery(AT, fun at_misc:id/1),
    aborted = at_server:update_t(AT, Ref1, fun at_misc:id/1),
    aborted = at_server:query_t(AT, Ref1, fun at_misc:id/1),

    aborted = at_server:update_t(AT, Ref0, fun at_misc:id/1),
    aborted = at_server:query_t(AT, Ref0, fun at_misc:id/1),
    aborted = at_server:commit_t(AT, Ref0),

    {ok, muuuuh} = at_server:stop(AT).


%% Test that concurrent transactions seem to work.
concurrent_transactions_test() ->
    {ok, AT} = at_server:start(start),
    {ok, Ref0} = at_server:begin_t(AT),
    {ok, Ref1} = at_server:begin_t(AT),
    {{NRef0, T0}, {NRef1, T1}} =
        case random:uniform(2) =:= 1 of
            true -> {{Ref0, ref0}, {Ref1, ref1}};
            false -> {{Ref1, ref1}, {Ref0, ref0}}
        end,
    S = self(),
    spawn(fun() ->
                  at_server:update_t(AT, NRef0, fun(_) -> T0 end),
                  S ! {T0, at_server:commit_t(AT, NRef0)}
          end),
    spawn(fun() ->
                  at_server:update_t(AT, NRef1, fun(_) -> T1 end),
                  S ! {T1, at_server:commit_t(AT, NRef1)}
          end),
    {RA, SA} = receive T -> T end,
    {RB, SB} = receive U -> U end,
    
    ?assert((SA =:= ok andalso SB =:= aborted)
            orelse (SB =:= ok andalso SA =:= aborted)),
    AW =
        case SA =:= ok of
            true -> RA;
            false -> RB
        end,
    aborted = at_server:commit_t(AT, Ref0),
    aborted = at_server:update_t(AT, Ref0, fun at_misc:id/1),
    aborted = at_server:query_t(AT, Ref1, fun at_misc:id/1),
    aborted = at_server:commit_t(AT, Ref1),

    {ok, AW} = at_server:doquery(AT, fun at_misc:id/1),
    {ok, AW} = at_server:stop(AT).


%% Test tryUpdate.
tryUpdate_test() ->
    {ok, AT} = at_server:start(start),
    S = self(),
    spawn(fun() -> aborted = at_extapi:tryUpdate(
                               AT, fun(_) -> S ! hullo, 
                                             timer:sleep(100),
                                             ref0
                                   end),
                   S ! at_server:stop(AT)
          end),
    receive hullo -> ok end,
    ok = at_extapi:tryUpdate(AT, fun(_) -> ref1 end),
    receive {ok, ref1} -> ok end.
    

%% Test ensureUpdate.
ensureUpdate_test() ->
    {ok, AT} = at_server:start(start),
    Xs = lists:seq(1, 100),
    S = self(),
    lists:map(fun(X) -> spawn(
                          fun() -> S ! at_extapi:ensureUpdate(
                                         AT, fun(_) -> X end) end) end, Xs),
    As = lists:map(fun(_) -> ok end, Xs),
    Bs = lists:map(fun(_) -> receive T -> T end end, Xs),
    As = Bs,
    {ok, N} = at_server:doquery(AT, fun at_misc:id/1),
    ?assert(lists:member(N, Xs)),
    at_server:stop(AT).


%% Test choiceUpdate.
choiceUpdate_test() ->
    State = 5,
    {ok, AT} = at_server:start(State),
    Fun = fun(S, E) -> S + E end,
    Elements = lists:seq(1, 336),
    Res = at_extapi:choiceUpdate(AT, Fun, Elements),
    ?assert(lists:member(Res, lists:map(fun(E) -> Fun(State, E) end, Elements))).
