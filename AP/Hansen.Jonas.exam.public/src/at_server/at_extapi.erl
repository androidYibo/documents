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

-module(at_extapi).
-include_lib("eunit/include/eunit.hrl").

-export([abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3, choiceUpdate/4]).

-define(SERVER_MODULE, at_server).
-import(?SERVER_MODULE, [start/1, stop/1, begin_t/1, doquery/2,
                         query_t/3, update_t/3, commit_t/2]).

%%%-------------------------------------------------------------------
%%% Extended API
%%%-------------------------------------------------------------------

abort(AT, Ref) ->
  Fail = fun (_) -> throw(abortMe) end,
  update_t(AT, Ref, Fail).

tryUpdate(AT, Fun) -> 
  Me = self(),
  {ok, Ref} = begin_t(AT),
  Upd = fun (S) -> 
    try Fun(S) of
      S1 ->
        Me ! {updated, Ref},
        S1
    catch 
      _ : Why -> 
        Me ! {failed, Ref},
        throw(Why)
    end
  end,
  update_t(AT, Ref, Upd),
  receive 
    {updated, Ref} -> commit_t(AT, Ref);
    {failed, Ref} -> error
  end.

ensureUpdate(AT, Fun) ->
  case tryUpdate(AT, Fun) of
    aborted ->
      ensureUpdate(AT, Fun);
    X -> X % is either error or ok, both of which 
           % are also acceptable results here
  end.
  
choiceUpdate(AT, Fun, Val_list) -> 
  choiceUpdateWork(AT, Fun, Val_list),
  receive % first one that messages back, wins
    {committed_t, Result} -> Result
  end.
  
choiceUpdate(AT, Fun, Val_list, Timeout) -> 
  choiceUpdateWork(AT, Fun, Val_list),
  receive % first one that messages back, wins
    {committed_t, Result} -> Result
  after
    Timeout -> timeout
  end.

% Long, and complicated version
% commitWorker(AT, Ref, Parent) ->
  % CommitRes = commit_t(AT, Ref),
  % Parent ! {committed_t, CommitRes}.
% choiceUpdateWork(AT, Fun, Val_list) -> 
  % TransactionWorker = % produce a fun to apply to state within transaction process
    % fun (Parent, E, Ref) ->
      % fun(S) -> % this works inside the transaction process
        % Res = Fun(S, E), % may fail, but let transaction handle it
        % % spawn someone to commit (we cannot do it, for risk of deadlock!), 
        % spawn(fun()-> commitWorker(AT, Ref, Parent) end),
        % % and then return my new state
        % Res
        % % even though we spawn now, server cannot complete the commit 
        % % before we finish, so state will be updated for commit
      % end
    % end,

  % Me = self(),
  % Launch = 
    % fun(Parent, E) ->
      % {ok, Ref} = begin_t(AT),
      % update_t(AT, Ref, TransactionWorker(Parent, E, Ref))
    % end,
  % _ = [spawn(fun() -> Launch(Me, E) end) || E <- Val_list].
  
% Simpler version, works
choiceUpdateWork(AT, Fun, Val_list) -> 
  Launch = fun(Parent, E) ->
    Fun1 = fun(S) -> Fun(S, E) end,
    case tryUpdate(AT, Fun1) of
      error -> ok; % update failed, silently die
      CommitRes -> Parent ! {committed_t, CommitRes}
    end
  end,
  Me = self(),
  _ = [spawn(fun() -> Launch(Me, E) end) || E <- Val_list].

%%%-------------------------------------------------------------------
%%% BLACK-BOX UNIT TESTS FOR EXTAPI
%%%-------------------------------------------------------------------

basic_funs() ->
  {fun(X) -> X end
  ,fun(X) -> X + 1 end
  ,fun(X) -> X + bad end
  ,fun(_) -> throw(fit) end}.

abort_test() ->
  {Id, _, _, _} = basic_funs(),
  {ok, Server} = start(42),
  {ok, Ref} = begin_t(Server),
  
  abort(Server, Ref),
  aborted = query_t(Server, Ref, Id),
  
  {ok, 42} = stop(Server).
  
tryUpdate_test() ->
  {_, Inc, Fail, Throw} = basic_funs(),
  {ok, Server} = start(42),
  error = tryUpdate(Server, Fail),
  error = tryUpdate(Server, Throw),
  ok = tryUpdate(Server, Inc),
  
  Me = self(),
  Master = spawn(fun()-> 
    receive {hello, Slave} ->
      tryUpdate(Server, Inc),
      Slave ! goAhead
    end
  end),
  
  SlowUpd = fun(X) ->
    Master ! {hello, self()}, 
    receive goAhead -> X + 1 end
    
  end,
  spawn(fun() -> Me ! tryUpdate(Server, SlowUpd) end),
  
  receive
    X -> ?assert(X =:= aborted)
  end,

  {ok, 44} = stop(Server).

ensureUpdate_test() ->
  {_, Inc, Fail, Throw} = basic_funs(),
  {ok, Server} = start(42),
  error = ensureUpdate(Server, Fail),
  error = ensureUpdate(Server, Throw),
  ok = ensureUpdate(Server, Inc), % server state = 43
  
  Me = self(),
  Master = spawn(fun()-> 
    receive
      Slave -> 
        ensureUpdate(Server, Inc),
        Slave ! goAhead,
        receive NewSlave -> NewSlave ! goAhead end
    end
  end),
  
  SlowUpd = fun(X) -> Master ! self(), receive goAhead -> X + 1 end end,
  spawn(fun() -> Me ! ensureUpdate(Server, SlowUpd) end),
  
  receive
    X -> ?assert(X =:= ok)
  end,

  {ok, 45} = stop(Server).
  
% To ensure deterministic testing, we must have some updates wait for others
% Testing assumes that at least one function successfully updates the state
choiceUpdate_test() ->
  {Id, _, _, _} = basic_funs(),
  {ok, Server} = start(42),
  
  Me = self(),
  Master = spawn(fun() -> receive {two, PID} -> PID ! goAhead end end),
  Choices = [
    fun(S) -> Me     ! {one, self()}, receive goAhead -> S + 1 end end,
    fun(S) -> Master ! {two, self()}, receive goAhead -> S + 2 end end,
    fun(S) -> Me     ! {three, self()}, receive goAhead -> S + 3 end end],
  Fun = fun(S, E) -> E(S) end,
  
  ok = choiceUpdate(Server, Fun, Choices),
  {ok, 44} = doquery(Server, Id),
  
  % % make remaining finish
  receive {one, PID1} -> PID1 ! goAhead end,
  receive {three, PID3} -> PID3 ! goAhead end,
  {ok, 44} = stop(Server).
  
choiceUpdate2_test() ->
  {ok, Server} = start(42),
  
  Me = self(),
  Master = spawn(fun() -> 
    receive ready -> receive {two, PID} -> PID ! goAhead end end end),
  Choices = [
    fun(S) -> Me     ! {one, self()}, receive goAhead -> S + 1 end end,
    fun(S) -> Master ! {two, self()}, receive goAhead -> S + 2 end end,
    fun(S) -> Me     ! {three, self()}, receive goAhead -> S + 3 end end],
  Fun = fun(S, E) -> E(S) end,

  spawn(fun() -> Me ! choiceUpdate(Server, Fun, Choices) end),
  
  % all are blocked, so update while they wait  
  tryUpdate(Server, fun(S) -> S + 7 end),
  
  % make remaining finish
  Master ! ready,
  receive {one, PID1} -> PID1 ! goAhead end,
  receive {three, PID3} -> PID3 ! goAhead end,
  
  % get result
  receive X2 -> ?assert(X2 =:= aborted) end,  
  
  {ok, 49} = stop(Server). 

