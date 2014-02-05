%%%-------------------------------------------------------------------
%%% @author Niels G. W. Serup <ngws@metanohi.name>
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Atomic Transaction Server API.
%%% @end
%%% Created: Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name: Niels G. W. Serup <ngws@metanohi.name>
%%%-------------------------------------------------------------------

-module(at_server).

-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3,
         update_t/3, commit_t/2]).

-type state_transformer() :: fun((any()) -> any()).


-spec start(State::any()) -> {ok, AT::pid()}.
%% @doc
%% Create an atomic transaction server `AT' with `State' as the initial state.
%% @end
start(State) ->
    gen_fsm:start_link(at_server_master, {State, [], 0}, []).


-spec stop(AT::pid()) -> {ok, State::any()}.
%% @doc
%% Abort all running transactions in the transaction server `AT' and stop
%% itself.  Return the current `State' of the server.
%% @end
stop(AT) ->
    {ok, gen_fsm:sync_send_all_state_event(AT, stop)}.


-spec doquery(AT::pid(), Fun::state_transformer())
             -> ({ok, StateNew::any()} | error).
%% @doc
%% Query the server with `Fun' on its current `State'.  Return the result, or
%% `error' if `Fun' fails.
%% @end
doquery(AT, Fun) ->
    gen_fsm:sync_send_event(AT, {doquery, Fun}).


-spec begin_t(AT::pid()) -> {ok, Ref::pid()}.
%% @doc
%% Start a new atomic transaction on `AT'.  Return the new unique transaction
%% id.
%% @end
begin_t(AT) ->
    {ok, gen_fsm:sync_send_event(AT, fork)}.


-spec query_t(AT::pid(), Ref::pid(), Fun::state_transformer())
             -> ({ok, StateNew::any()} | aborted).
%% @doc
%% Query the helper transaction `Ref' with `Fun' on its current `State'.  Return
%% the result, or `aborted' if `Fun' fails (in which case the transaction is
%% also aborted), if the transaction has already been aborted, or if the
%% transaction does not exist.
%% @end
query_t(AT, Ref, Fun) ->
    aborted_if_error(fun() -> gen_fsm:sync_send_event(Ref, {doquery, Fun, AT}) end).


-spec update_t(AT::pid(), Ref::pid(), Fun::state_transformer())
              -> (ok | aborted).
%% @doc
%% Update the `State' of the helper transaction `Ref' with `Fun'.  Return
%% `aborted' if the transaction does not exist, otherwise `ok'.
%% @end
update_t(AT, Ref, Fun) ->
    case erlang:is_process_alive(Ref) of
        true -> aborted_if_error(
                  fun() -> gen_fsm:send_event(Ref, {update, Fun, AT}) end);
        false -> aborted
    end.


-spec (commit_t(AT::pid(), Ref::pid()) -> (ok | aborted)).
%% @doc
%% Commit the state of `Ref' to `AT'.  Return `ok' if successful, otherwise
%% `aborted'.
%% @end
commit_t(AT, Ref) ->
    aborted_if_error(fun() -> gen_fsm:sync_send_event(Ref, {commit, AT}) end).


-spec aborted_if_error(Fun::fun(() -> any())) -> (any() | aborted).
%% @doc Returns `aborted' if an error occurs.
aborted_if_error(Fun) ->
    try Fun()
    catch
        _:_ -> aborted
    end.
