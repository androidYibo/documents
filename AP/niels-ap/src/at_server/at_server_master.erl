%%%-------------------------------------------------------------------
%%% @author Niels G. W. Serup <ngws@metanohi.name>
%%% @doc
%%% Implementation of the atomic transaction server.
%%% @end
%%% Created: Nov 2013 by Niels G. W. Serup <ngws@metanohi.name>
%%%-------------------------------------------------------------------
%%% Student name: Niels G. W. Serup <ngws@metanohi.name>
%%%-------------------------------------------------------------------

-module(at_server_master).
-behaviour(gen_fsm).


-export([init/1, handle_event/3, terminate/3, handle_info/3,
         code_change/4, handle_sync_event/4]).
-export([ready/3]).


%%%-------------------------------------------------------------------
%%% Unused gen_fsm callbacks.
%%%-------------------------------------------------------------------

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_event(_Event, _StateName, State) ->
    {stop, exit, State}.


%%%-------------------------------------------------------------------
%%% Used gen_fsm callbacks and helpers.
%%%-------------------------------------------------------------------

-type master_state() :: {S::any(), Helpers::[pid()], Iter::integer()}.


init(State) ->
    {ok, ready, State}.


%% @doc Abort all transactions and stop.
handle_sync_event(stop, _From, _StateName, State = {S, Helpers, _Iter}) ->
    abort_helpers(Helpers),
    {stop, normal, S, State}.


%% @doc Abort all transactions.
abort_helpers(Helpers) ->
    lists:map(fun(H) -> at_extapi:abort(self(), H) end, Helpers).


%% @doc Query, fork, or commit.
-spec (ready(Action::any(), From::pid(), State::master_state())
       -> NewAction::any()).
ready(Action, _From, State) ->
    {Reply, NextState} =
        case Action of
            {doquery, Fun} -> doquery(State, Fun);
            fork -> fork(State);
            {commit, S1, HelperIter} -> commit(State, S1, HelperIter)
        end,
    {reply, Reply, ready, NextState}.


-spec doquery(State::master_state(), Fun::at_server:state_transformer())
             -> {({ok, StateNew::master_state()} | error), master_state()}.
%% @doc Query the server.
doquery(State = {S, _Helpers, _Iter}, Fun) ->
    R = try Fun(S) of
            S1 -> {ok, S1}
        catch
            _:_ -> error
        end,
    {R, State}.


-spec fork(State::master_state()) -> {Ref::pid(), StateNew::master_state()}.
%% @doc Create a new helper transaction.
fork({S, Helpers, Iter}) ->
    {ok, Ref} = gen_fsm:start_link(at_server_helper, {S, self(), Iter}, []),
    {Ref, {S, [Ref | Helpers], Iter}}.


-spec commit(State::master_state(), S1::any(), HelperIter::integer())
            -> {ok, NewState::master_state()}.
%% @doc Try to commit a result from a helper transaction.
commit(State = {_S, Helpers, Iter}, S1, HelperIter) ->
    case HelperIter =:= Iter of
        true ->
            abort_helpers(Helpers),
            {ok, {S1, [], Iter + 1}};
       false ->
            {aborted, State}
    end.
