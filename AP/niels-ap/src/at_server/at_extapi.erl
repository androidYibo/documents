%%%-------------------------------------------------------------------
%%% @author Niels G. W. Serup <ngws@metanohi.name>
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Atomic Transaction Server Extended API.
%%% @end
%%% Created: Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name: Niels G. W. Serup <ngws@metanohi.name>
%%%-------------------------------------------------------------------

-module(at_extapi).

-export([abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).


-spec abort(AT::pid(), Ref::pid()) -> aborted.
%% @doc Force an abort of `Ref' on `AT'.
abort(AT, Ref) ->
    at_server:update_t(AT, Ref, fun() -> erlang:error(aborted) end),
    aborted.


-spec (tryUpdate(AT::pid(), Fun::at_server:state_transformer())
       -> (ok | error | aborted)).
%% @doc
%% Try to update the `State' on `AT' with `Fun'.  Return `ok' on success,
%% `error' if `Fun' fails, and `aborted' if another transaction committed before
%% this one did.
%% @end
tryUpdate(AT, Fun) ->
    {ok, Ref} = at_server:begin_t(AT),
    at_server:update_t(AT, Ref, Fun),
    at_server:commit_t(AT, Ref).


-spec (ensureUpdate(AT::pid(), Fun::at_server:state_transformer())
       -> {ok, error}).
%% @doc
%% Has the same `ok' and `error' effects as `tryUpdate', but always comes
%% through with its commit (so it never returns `aborted').
%% @end
%% @see tryUpdate
ensureUpdate(AT, Fun) ->
    Res = tryUpdate(AT, Fun),
    case Res of
        aborted -> ensureUpdate(AT, Fun);
        T -> T
    end.


-spec (choiceUpdate(AT::pid(),
                    Fun::fun((State::any(), E::any()) 
                             -> NewState::any()), [any()]) -> (FinalState::any() | error)).
%% Commit and return the `NewState' whose `Fun' finishes first.  Abort the rest.
%% However, if all `Fun' applications return `error', then return `error'.
choiceUpdate(AT, Fun, Elements) ->
    S = self(),
    %% The actual core function might have some spawned processes sending data
    %% back before they are killed, but we don't want that data ending up in
    %% this process, so we start a new one.  This could most likely be done
    %% better.
    spawn(fun() -> choiceUpdate1(AT, Fun, Elements, S) end),
    receive V -> V end.

choiceUpdate1(AT, Fun, Elements, ReplyTo) ->
    {ok, S} = at_server:doquery(AT, fun at_misc:id/1),
    Self = self(),
    Pids = lists:map(fun(E) -> spawn(fun() -> Self ! try Fun(S, E) 
                                                     of V -> {ok, V}
                                                     catch _:_ -> error
                                                     end
                                     end) end, Elements),
    Res = choiceUpdateRec(length(Elements)),
    lists:map(fun(P) -> exit(P, kill) end, Pids),
    case Res =/= error of
        true -> ensureUpdate(AT, fun(_) -> Res end);
        false -> ok
    end,
    ReplyTo ! Res.

choiceUpdateRec(M) ->
    receive
        {ok, V} -> V;
        error -> case M > 1 of
                     true -> choiceUpdateRec(M - 1);
                     false -> error
                 end
    end.

