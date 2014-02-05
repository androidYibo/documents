%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Hand-in for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name: Jonas Stig Kaempf Hansen
%%% Student KU-id: 
%%%-------------------------------------------------------------------

-module(at_server).
-include_lib("eunit/include/eunit.hrl").

-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).


% debug(Msg, Args) ->
  % io:format("[DEBUG - ~p]: " ++ Msg, [self()|Args]).
debug(_, _) -> ok.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start(State) ->
    {ok, spawn(fun() -> initATServer(State) end)}.

% Blocking
stop(AT) ->
  call_stop(AT).

% Blocking
doquery(AT, Fun) ->
  call_query(AT, Fun).

% Non-blocking
begin_t(AT) -> 
  Ref = make_ref(),
  cast_begin_t(AT, Ref),
  {ok, Ref}.

% Blocking
query_t(AT, Ref, Fun) ->
  call_query_t(AT, Ref, Fun).

% Non-blocking
update_t(AT, Ref, Fun) ->
  cast_update_t(AT, Ref, Fun).

% Blocking
commit_t(AT, Ref) ->
  call_commit_t(AT, Ref).

%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).

reply_error(From) ->
    reply(From, error).

reply_abort(From) ->
    reply(From, aborted).
    
call_stop(AT) -> rpc(AT, stop).
call_query(To, Fun) -> rpc(To, {doquery, Fun}).
call_query_t(AT, Ref, Fun) -> rpc(AT, {query_t, Ref, Fun}).
call_commit_t(AT, Ref) -> rpc(AT, {commit_t, Ref}).

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

cast_begin_t(AT, Ref) ->
  info(AT, {begin_t, Ref}).
cast_update_t(AT, Ref, Fun) -> 
  info(AT, {update_t, Ref, Fun}).
cast_update(TID, Ref, Fun) -> 
  info(TID, {self(), {update, Ref, Fun}}).
cast_update_error(AT, Ref) ->
  info(AT, {updateFailed, Ref}).
cast_stop(TID) ->
  info(TID, {self(), stop}).
  
%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

%%% AT Server loop

initATServer(State) ->
  atServerLoop(State, []).

atServerLoop(State, Trans) ->
  receive
    {From, {doquery, Fun}} ->
      Result = safeQuery(Fun, State),
      reply(From, Result),
      atServerLoop(State, Trans);

    {begin_t, Ref} ->
      Trans1 = [{Ref, newTrans(State)} | Trans],
      atServerLoop(State, Trans1);

    {From, {query_t, Ref, Fun}} ->
      TID = lookupTransaction(Ref, Trans),
      Trans1 = 
        case queryTransaction(TID, Fun) of
          error -> 
            reply_abort(From),
            abortTransaction(Ref, Trans);
            
          {ok, Result} -> 
            reply_ok(From, Result),
            Trans
        end,
      atServerLoop(State, Trans1);

    {update_t, Ref, Fun} ->
      TID = lookupTransaction(Ref, Trans),
      updateTransaction(TID, Ref, Fun),
      atServerLoop(State, Trans);
      
    {updateFailed, Ref} ->
      Trans1 = abortTransaction(Ref, Trans),
      atServerLoop(State, Trans1);

    {From, {commit_t, Ref}} ->
      TID = lookupTransaction(Ref, Trans),
      Id = fun (X) -> X end,
      {State1, Trans1} = 
        case queryTransaction(TID, Id) of
          error ->
            reply_abort(From),
            {State, abortTransaction(Ref, Trans)};
          {ok, S} -> 
            reply_ok(From),
            {S, abortAllTransactions(Trans)}
        end,
      atServerLoop(State1, Trans1);

    {From, stop} ->
      abortAllTransactions(Trans),
      reply_ok(From, State)
  end.

%%% Transaction loop

initTransaction(Server, State) ->
  transactionLoop(Server, State).
  
transactionLoop(Server, State) ->
  receive
    {Server, {doquery, Fun}} ->
      debug("Query received~n", []),
      reply(Server, safeQuery(Fun, State)),
      transactionLoop(Server, State);
    
    {Server, {update, Ref, Fun}} ->
      case safeUpdate(Fun, State) of
        error -> 
          cast_update_error(Server, Ref),
          transactionFailed(Server);
        {ok, State1} -> 
          transactionLoop(Server, State1)
      end;
    
    {Server, stop} ->
      debug("Transaction stopping in State ~p~n", [State]),
      ok
  end.
  
transactionFailed(Server) ->
  receive
    {Server, {doquery, _}} ->
      reply_error(Server),
      transactionFailed(Server);
    {Server, stop} -> ok
  end.
  
%%% Helper functions
  
% Returns {ok, Fun(State)}, or error if Fun(State) fails
safeQuery(Fun, State) ->
  try Fun(State) of
    Result -> 
      debug("Query on State ~p successful. Result: ~p.~n", [State, Result]),
      {ok, Result}
  catch
    _ : Why ->
      debug("Query on State ~p failed. Reason: ~p~n", [State, Why]),
      error
  end.

% Returns the updated state as {ok, State1}, or error
safeUpdate(Fun, State) -> safeQuery(Fun, State).

% spawn new transaction
newTrans(State) ->
  Server = self(),
  spawn(fun() -> initTransaction(Server, State) end).
  
% Lookup transaction Ref and get transaction process id
lookupTransaction(_, []) -> aborted;
lookupTransaction(Ref, [{Ref, TID} | _]) -> TID;
lookupTransaction(Ref, [_ | Trans]) -> lookupTransaction(Ref, Trans).

% Abort transaction Ref from list of transactions
abortTransaction(_, []) -> [];
abortTransaction(Ref, [{Ref, TID} | Trans]) ->
  cast_stop(TID),
  Trans;
abortTransaction(Ref, [T | Trans]) ->
  [T | abortTransaction(Ref, Trans)].

abortAllTransactions([]) -> [];
abortAllTransactions([{_, TID} | Trans]) ->
  cast_stop(TID),
  abortAllTransactions(Trans).

% Queries a transaction, get either {ok, Result}, or error
queryTransaction(aborted, _) -> error;
queryTransaction(TID, Fun) -> call_query(TID, Fun).
  
updateTransaction(aborted, _, _) -> error;
updateTransaction(TID, Ref, Fun) -> 
  cast_update(TID, Ref, Fun). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BLACK BOX TESTING OF API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Returns tuple of functions \x -> A, 
% where A is (in the order returned) {x, x + 1, x + bad, throw(fit)}
% Suggested use: {Id, Inc, Fail, Throw} = basic_funs()
basic_funs() ->
  {fun(X) -> X end
  ,fun(X) -> X + 1 end
  ,fun(X) -> X + bad end
  ,fun(_) -> throw(fit) end}.

%%% Test cases
  
start_server_test() ->
  {ok, S} = start(0), stop(S).
  
simple_stop_server_test() ->
  {ok, Server} = start(42),
  {ok, 42} = stop(Server).
  
do_single_query_test() -> 
  {ok, Server} = start(0),
  {_, Inc, _, _} = basic_funs(),
  {ok, 1} = doquery(Server, Inc),
  {ok, 0} = stop(Server).
  
do_more_query_test() -> 
  {ok, Server} = start(0),
  {_, Inc, _, _} = basic_funs(),
  {ok, 1} = doquery(Server, Inc),
  {ok, 1} = doquery(Server, Inc),
  {ok, 0} = stop(Server).
  
fail_fun_doquery_test() ->
  {ok, Server} = start(0),
  {_, _, Wrong, _} = basic_funs(),
  error = doquery(Server, Wrong),
  {ok, 0} = stop(Server).
  
throw_fun_doquery_test() ->
  {ok, Server} = start(0),
  {_, _, _, Throw} = basic_funs(),
  error = doquery(Server, Throw),
  {ok, 0} = stop(Server).  
  
% Note this is not intended as testing of general uniqueness of refs
begin_t_test() ->
  {ok, Server} = start(42),
  {ok, Ref1} = begin_t(Server),
  {ok, Ref2} = begin_t(Server),
  ?assert(Ref1 =/= Ref2),
  {ok, 42} = stop(Server).
  
query_t_test() ->
  {ok, Server} = start(42),
  
  % Transaction 1
  {ok, Ref1} = begin_t(Server),
  {_, Inc, Wrong, Fail} = basic_funs(),
  {ok, 43} = query_t(Server, Ref1, Inc),
  
  aborted = query_t(Server, Ref1, Wrong),
  aborted = query_t(Server, Ref1, Inc),
  
  % Transaction 2
  {ok, Ref2} = begin_t(Server),
  {ok, 43} = query_t(Server, Ref2, Inc),
  
  aborted = query_t(Server, Ref2, Fail),
  aborted = query_t(Server, Ref2, Inc),  
  
  {ok, 42} = stop(Server).  

update_t_test() ->
  {ok, Server} = start(42),
  {Id, Inc, Fail, Throw} = basic_funs(),
  
  {ok, Ref1} = begin_t(Server),
  
  update_t(Server, Ref1, Id),
  {ok, 42} = query_t(Server, Ref1, Id),
  
  update_t(Server, Ref1, Inc),
  {ok, 43} = query_t(Server, Ref1, Id),
  {ok, 42} = doquery(Server, Id),
  
  update_t(Server, Ref1, Fail),
  aborted = query_t(Server, Ref1, Id),
  {ok, 42} = doquery(Server, Id),
  
  {ok, Ref2} = begin_t(Server),
  {ok, 42} = query_t(Server, Ref2, Id),
  update_t(Server, Ref2, Throw),
  aborted = query_t(Server, Ref2, Id),
  {ok, 42} = doquery(Server, Id),
  
  % update on aborted, does not crash anything
  update_t(Server, Ref2, Inc),
  {ok, 42} = doquery(Server, Id),
  
  {ok, 42} = stop(Server). 

commit_t_test() ->
  {ok, Server} = start(42),
  {Id, Inc, Fail, _} = basic_funs(),
  
  {ok, Ref1} = begin_t(Server),
  {ok, Ref2} = begin_t(Server),
  
  update_t(Server, Ref1, Inc),
  update_t(Server, Ref1, Inc), % Ref1 == 44
  update_t(Server, Ref2, Inc), % Ref2 == 43
  
  {ok, 42} = doquery(Server, Id),
  ok = commit_t(Server, Ref1),
  aborted = commit_t(Server, Ref2),
  {ok, 44} = doquery(Server, Id),
  aborted = commit_t(Server, Ref1),
  
  aborted = query_t(Server, Ref1, Id),
  aborted = query_t(Server, Ref2, Id),
  
  {ok, Ref3} = begin_t(Server),
  update_t(Server, Ref3, Fail),
  aborted = commit_t(Server, Ref3),
  
  {ok, 44} = stop(Server). 
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PARTIAL WHITE BOX TESTING OF INTERNAL IMPLEMENTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
  
lookup_t_test() ->
  aborted = lookupTransaction(42, []),
  Ts = [{4, hitchhikers}, {2, guide}, {6, galaxy}],
  hitchhikers = lookupTransaction(4, Ts),
  guide = lookupTransaction(2, Ts),
  galaxy = lookupTransaction(6, Ts),
  aborted = lookupTransaction(7, Ts).
  
abort_t_test() ->  
  [] = abortTransaction(42, []),
  TID = self(),
  Ts = [{4, TID}, {2, TID}, {100, TID}],
 
  Ts = abortTransaction(0, Ts), % abort aborted trans

  Ts1 = abortTransaction(4, Ts), % abort head of list
  [{2, TID}, {100, TID}] = Ts1,
  
  Ts1 = abortTransaction(4, Ts1), % abort same again
  
  Ts2 = abortTransaction(2, Ts), % abort some middle
  [{4, TID}, {100, TID}] = Ts2,
  
  Ts3 = abortTransaction(100, Ts), % abort tail
  [{4, TID}, {2, TID}] = Ts3,
  
  Ts4 = abortTransaction(2, Ts3), % abort last of two
  [{4, TID}] = Ts4,
  
  Ts5 = abortTransaction(4, Ts4), % single
  [] = Ts5.
  