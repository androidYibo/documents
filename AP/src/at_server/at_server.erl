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

-module(at_server).

-export([start/1, stop/1, doquery/2, begin_t/1, query_t/3, update_t/3, commit_t/2]).
%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start(State) ->
    {ok, spawn(fun() -> at(State, []) end)}.

stop(AT) ->
    rpc(AT, stop).
    
doquery(AT, Fun) ->
    rpc(AT, {doquery, Fun}).
    
begin_t(AT) ->
    rpc(AT, begin_t).

query_t(AT, Ref, Fun) ->
    rpc(AT, {query_t, Ref, Fun}).

update_t(AT, Ref, Fun) ->
    info(AT, {update_t, Ref, Fun}).

commit_t(AT, Ref) ->
    rpc(AT, {commit_t, Ref}).

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

at(State, Refs) ->
    receive
        {From, stop} ->
            sendToAllRefs(stop, Refs),
            reply_ok(From, State);
        {From, {doquery, Fun}} ->
            try Fun(State) of
                Result -> reply_ok(From, Result)
            catch _:_ -> reply_error(From)
            end,
            at(State, Refs);
        {From, begin_t} ->
            Ref = erlang:make_ref(),
            ATPID = self(),
            Pid = spawn(fun() -> trans(State, ATPID) end), %The coordinator creates and holds
            reply_ok(From, Ref),                           %the refs to have access to it's 
            at(State, [{Ref,Pid}] ++ Refs);                %children. The PIDS are saved with refs
        {From, {query_t, Ref, Fun}} ->
            case proplists:get_value(Ref, Refs) of
                undefined -> reply_abort(From);
                PID ->  reply(From, rpc(PID, {query_t, Fun}))
            end,
            at(State, Refs);
        {update_t, Ref, Fun} ->
            case proplists:get_value(Ref, Refs) of
               undefined -> found_no_ref;
               PID -> info(PID, {update_t, Fun})
            end,
            at(State, Refs);
        {From, {commit_t, Ref}} ->
            case proplists:get_value(Ref, Refs) of     
               undefined -> reply_abort(From),
                            NewState = State;
               PID -> TransState = rpc(PID, commit_t), %I am asuming a failed commit_t is not 
                      case TransState of               %supposed to abort all trans
                        aborted -> NewState = State;
                        Changed -> NewState = Changed
                      end,
                      sendToAllRefs(stop, Refs)
            end,
            reply_ok(From),
            at(NewState, Refs);
        {From, aborted} ->
            NewRefs = removeReffromPID(From, Refs), %removes transactions from Refs as 
            at(State, NewRefs)                      %they are aborted
    end.

trans(State, AT) ->
    receive
        stop ->
            reply(AT, aborted);
        {From, {query_t, Fun}} ->
            try Fun(State) of
                Result -> reply_ok(From, Result),
                          trans(State, AT)
            catch _:_ -> reply_abort(From)
            end;
        {update_t, Fun} ->
            try Fun(State) of
                Result -> trans(Result, AT)
            catch _:_ -> reply_abort(AT)
            end;
        {From, commit_t} ->
            case State of
                aborted  -> reply(AT, aborted);
                Anything -> reply(From, Anything)
            end
    end.

%%%-------------------------------------------------------------------
%%% Helper Functions
%%%-------------------------------------------------------------------
   
sendToAllRefs(_, []) -> ok; 
sendToAllRefs(Msg, Refs) ->
    {_,Pid} = hd(Refs),
    info(Pid, Msg),
    sendToAllRefs(Msg, tl(Refs)).

    
removeReffromPID(_, []) -> []; %reinventing the deep dish    
removeReffromPID(PID, Refs) ->
    {_,PID2} = hd(Refs),
    if PID == PID2 -> tl(Refs);
       true -> [hd(Refs)] ++ removeReffromPID(PID, tl(Refs))
    end.
%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% synchronous communication

rpc(Pid, Request) ->
	info(Pid, {self(), Request}),
    receive
    {Pid, Response} ->
        Response
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

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

