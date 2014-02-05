%%%-------------------------------------------------------------------
%%% @author Niels G. W. Serup <ngws@metanohi.name>
%%% @doc
%%% Implementation of the atomic transaction helper server.
%%% @end
%%% Created: Nov 2013 by Niels G. W. Serup <ngws@metanohi.name>
%%%-------------------------------------------------------------------
%%% Student name: Niels G. W. Serup <ngws@metanohi.name>
%%%-------------------------------------------------------------------

-module(at_server_helper).
-behaviour(gen_fsm).


-export([init/1, handle_event/3, terminate/3, handle_info/3,
         code_change/4, handle_sync_event/4]).
-export([ready/2, ready/3]).


%%%-------------------------------------------------------------------
%%% Unused gen_fsm callbacks.
%%%-------------------------------------------------------------------

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_sync_event(_Event, _From, _StateName, State) ->
    {stop, exit, State}.


%%%-------------------------------------------------------------------
%%% Used gen_fsm callbacks and helpers.
%%%-------------------------------------------------------------------

-type helper_state() :: {S::any(), AT::pid(), Iter::integer()}.


init(State) ->
    {ok, ready, State}.


%% @doc Stop.
handle_event(stop, _From, State) ->
    {stop, normal, State}.


%% @doc Update.
ready(Action, State) ->
    case Action of
        {update, Fun, AT} -> update(State, Fun, AT)
    end.


%% @doc Query or commit.
ready(Action, _From, State) ->
    case Action of
        {doquery, Fun, AT} -> doquery(State, Fun, AT);
        {commit, AT} -> commit(State, AT)
    end.
    

-spec (update(State::helper_state(), Fun::at_server:state_transformer(),
              AT1::pid()) -> ({stop, term(), helper_state()}
                              | {next_state, ready, helper_state()})).
%% @doc Update the helper transaction.
update(State = {S, AT, Iter}, Fun, AT1) ->
    case AT =/= AT1 of
        true ->
            {stop, normal, State};
        false ->
            try Fun(S) of
                S1 -> {next_state, ready, {S1, AT, Iter}}
            catch
                _:_ -> {stop, normal, S}
            end
    end.


-spec (doquery(State::helper_state(), Fun::at_server:state_transformer(), 
               AT1::pid()) -> ({stop, term(), any(), helper_state()}
                               | {reply, any(), ready, helper_state()})).
%% @doc Query the helper transaction.
doquery(State = {S, AT, _Iter}, Fun, AT1) ->
    case AT =/= AT1 of
        true ->
            {stop, normal, aborted, State};
        false -> 
            try Fun(S) of
                S1 -> {reply, {ok, S1}, ready, State}
            catch
                _:_ -> {stop, normal, aborted, State}
            end
    end.


-spec (commit(State::helper_state(), AT1::pid())
       -> ({stop, term(), any(), helper_state()}
           | {reply, any(), ready, helper_state()})).
%% @doc Commit to the master.
commit(State = {S, AT, Iter}, AT1) ->
    case AT =/= AT1 of
        true ->
            {stop, normal, aborted, State};
        false -> 
            R = gen_fsm:sync_send_event(AT, {commit, S, Iter}),
            {stop, normal, R, State}
    end.
