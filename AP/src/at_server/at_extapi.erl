%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Skeleton for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name: Kasper Passov
%%% Student KU-id: pvx884
%%%-------------------------------------------------------------------

-module(at_extapi).

-export([abort/2, tryUpdate/2, ensureUpdate/2, choiceUpdate/3]).
-import(at_server, [start/1, stop/1, doquery/2, begin_t/1, query_t/3, update_t/3, commit_t/2]).

%%%-------------------------------------------------------------------
%%% Extended API
%%%-------------------------------------------------------------------

%returns abort
abort(AT, Ref) -> 
    AbortFun = 
        fun(State) ->
            State + 'a' + 0 %A function that always fails will force an abort
        end,
    query_t(AT, Ref, AbortFun).
    
%Returns ok, error or abort    
tryUpdate(AT, Fun) -> 
    {ok, Ref} = begin_t(AT),
    case query_t(AT, Ref, Fun) of
        {ok, _} -> update_t(AT, Ref, Fun),
                   Return = commit_t(AT, Ref);
        aborted -> Return = error
    end,
    Return.
    
%returns ok or abort    
ensureUpdate(AT, Fun) -> 
    case tryUpdate(AT, Fun) of
        abort -> ensureUpdate(AT, Fun); %Might be a better way, at the moment it just keeps trying if it gets an abort.
        Result -> Result
    end.

%returns the result of the commited Fun    
choiceUpdate(AT, Fun, Val_list) -> 
    PID = self(),
    Coord = spawn(fun() -> coordinator(PID, 0, false) end),
    spawn(fun() -> creator(AT, Fun, Val_list, Coord) end),
    receive
        Return -> Return
    end.
    
%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

coordinator(Parent, 0, true) -> info(Parent, everything_failed);
coordinator(Parent, NumberOfProcess, _) ->
    receive
        dead ->
            coordinator(Parent, NumberOfProcess - 1, true);
        new -> 
            coordinator(Parent, NumberOfProcess + 1, true);
        success ->
            info(Parent, ok)
    end.

updater(AT, Fun, Coord) ->
    case tryUpdate(AT, Fun) of
        error  -> info(Coord, dead);
        aborted  -> info(Coord, dead);
        ok -> info(Coord, success)
    end.
        
creator(_, _, [], _) -> done;   %creates length(Val_list) processes with the new fun 
creator(AT, Fun, Val_list, Coord) ->
    E = hd(Val_list),           %The though is to wrap in the list element into a new 
    NewFun =                    %function only taking a state as parameter
        fun(State) ->
            Fun(State, E)
        end,
    info(Coord, new),
    spawn(fun() -> updater(AT, NewFun, Coord) end),
    creator(AT, Fun, tl(Val_list), Coord).
        
        
%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.