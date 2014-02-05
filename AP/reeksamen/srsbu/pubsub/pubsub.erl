%%%-------------------------------------------------------------------
%%% @author Ken Friis Larsen <kflarsen@diku.dk>
%%% @copyright (C) 2014, Ken Friis Larsen
%%% @doc
%%% Skeleton for the re-exam for Advanced Programming, B1-2013
%%% Implementation of pubsub server
%%% @end
%%%-------------------------------------------------------------------
%%% Student name: Kasper Passov
%%% Student KU-id: pvx884
%%%-------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-module(pubsub).
-export([start/0, add_subscriber/3, stop/1, subscribers/1, publish/2, demonstration/0]).



%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start() -> 
    {ok, spawn(fun() -> psnode([],[],[]) end)}.

add_subscriber(P, S, F) ->
    rpc(P, {S, F, add_sub}).

subscribers(P) ->
    rpc(P, get_subs).

publish(P, E) -> 
    Ref = erlang:make_ref(),
    info(P, {{E, Ref}, publish}).

messages(P) ->
    rpc(P, messages).


%%%-------------------------------------------------------------------
%%% API extensions 
%%%-------------------------------------------------------------------
% These API extensions have been made to make testing easier

% The stop function kills the given process P. It does not kill or
% message its subscribers 
stop(P) ->
    rpc(P, stop). 

% Returns all errors the given process has saved. An error happens
% when a filter fails or returns something that is not a bool
errors(P) ->
    rpc(P, errors).
%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

psnode(Subs, Messages, Errors) ->
    receive
        {From, stop} ->
            reply(From, {stopped, self()}); 
        {From, errors} -> % return all errors
            reply(From, Errors);
        {Error, S, error} -> % save an error
            psnode(Subs, Messages, Errors ++ [{Error, S}]);
        {From, {S, F, add_sub}} -> % add a subscriber
            case lookupSubs(S, Subs) of 
                  none -> reply_ok(From), 
                          psnode([{S,F}] ++ Subs, Messages, Errors);
                  _ -> reply_error(From),
                       psnode(Subs, Messages, Errors)
            end;
        {From, get_subs} -> % return subscribers
            reply(From, getAll(Subs)),
            psnode(Subs, Messages, Errors);
        {{E, Ref}, publish} -> % forward message
            case lists:keyfind(Ref, 2, Messages) of
                false -> PassedList = lists:filter(fun({S,F}) -> tryFilter(self(),S,F,E) end, Subs),
                         lists:foreach(fun({S,_}) -> info(S, {{E, Ref}, publish}) end, PassedList),
                         psnode(Subs, [{E, Ref}] ++ Messages, Errors);
                _     -> psnode(Subs, Messages, Errors)
            end;
        {From, messages} -> % return all messages
            reply(From, getAll(Messages)),
            psnode(Subs, Messages, Errors);
        _ ->
            erlang:display(end_of_pubserver) % helps me find lost messages
    end.
%%%-------------------------------------------------------------------
%%% Helper Functions 
%%%-------------------------------------------------------------------

tryFilter(From, S, F, E) -> 
    try F(E) of 
        true -> true; 
        false -> false; % function fails 
        _ -> info(From, {filter_not_bool, S, error}),
             false %function does not return bool 
    catch % Funktion fails on type
    _ : _ -> info(From, {filter_fails, S, error}), 
                 false %"Bad filter function" 
end.

lookupSubs(_, []) -> none; 
lookupSubs(S, [{S, F} | _]) -> F;
lookupSubs(S, [_ | SUBS]) -> lookupSubs(S, SUBS). 

getAll([]) -> [];
getAll([{A, _} | Rest]) -> getAll(Rest) ++ [A].

%%%-------------------------------------------------------------------
%%% Demonstration 
%%%-------------------------------------------------------------------

% demonstration as describer on page 9 of the exam set
demonstration() ->
    {ok, Niels} = start(),
    {ok, Albert} = start(),
    {ok, Christiaan} = start(),
    {ok, Isaac} = start(),
    {ok, Joseph_Louis} = start(),
    {ok, Johannes} = start(),
    {ok, Euclid} = start(),
    add_subscriber(Albert, Niels, fun(_) -> true end),
    add_subscriber(Isaac, Albert, fun(X) -> hd(tl(X)) == 101 end), % 101 is the character 'e' in UNICODE
    add_subscriber(Christiaan, Albert, fun(_) -> true end),
    add_subscriber(Euclid, Isaac, fun(X) -> X rem 2 == 0 end),
    add_subscriber(Johannes, Isaac, fun(_) -> true end),
    add_subscriber(Isaac, Joseph_Louis, fun(_) -> true end),
    publish(Euclid, 5),
    publish(Euclid, 4),
    publish(Euclid, point),
    publish(Isaac, "Hello"),
    publish(Christiaan, {tick, tock}),
    publish(Albert, emc2),
    timer:sleep(10),
    {messages(Niels),messages(Joseph_Louis)}.


%%%-------------------------------------------------------------------
%%% Testing 
%%%-------------------------------------------------------------------

% tests start and stop
start_test() ->
    {ok, P} = start(),
    {stopped, P} = stop(P).

% tests publish and messages
publish_one_test() ->
    {ok, P} = start(),
    publish(P, "Winter"),
    publish(P, "is"),
    publish(P, "comming"),
    ["Winter","is","comming"] = messages(P).

% tests adding subscribers
add_sub_test() ->
    {ok, A} = start(),
    {ok, B} = start(),
    {ok, C} = start(),
    add_subscriber(A, B, fun(_) -> true end),
    add_subscriber(A, C, fun(_) -> true end),
    [B, C] = subscribers(A). 

% tests publishing to subscribers
publish_subs_test() ->
    {ok, A} = start(),
    {ok, B} = start(),
    {ok, C} = start(),
    add_subscriber(A, B, fun(_) -> true end),
    add_subscriber(A, C, fun(_) -> true end),
    error = add_subscriber(A, C, fun(_) -> true end),
    publish(A, "A is greatest"),
    timer:sleep(10),
    "A is greatest" = messages(A),
    "A is greatest" = messages(B),
    "A is greatest" = messages(C).

%tests filters
publish_filter_test() ->
    {ok, A} = start(),
    {ok, B} = start(),
    {ok, C} = start(),
    add_subscriber(A, B, fun(X) -> X < 4 end),
    add_subscriber(A, C, fun(X) -> X > 2 end),
    publish(A, 2),
    publish(A, 3),
    publish(A, 4),
    timer:sleep(10),
    AM = messages(A),
    BM = messages(B),
    CM = messages(C),
    {[2,3,4],[2,3],[3,4]} = {AM,BM,CM}.

% tests error handling and rebustness
error_function_test() ->
    {ok, A} = start(),
    {ok, B} = start(),
    {ok, C} = start(),
    add_subscriber(A, C, fun(_) -> throw(poo) end),
    add_subscriber(A, B, fun(X) -> X end),
    publish(A, "zoo abes throw "),
    timer:sleep(10),
    AE = errors(A),
    [{filter_not_bool,B}, {filter_fails,C}] = AE.

% message is recieved through nested sub
nested_subs_test() ->
    {ok, A} = start(),
    {ok, B} = start(),
    {ok, C} = start(),
    add_subscriber(A, B, fun(_) -> true end),
    add_subscriber(B, C, fun(_) -> true end),
    publish(A, "we can go deeper"),
    timer:sleep(10),
    ["we can go deeper"] = messages(C).

% messages are not continuously send
sub_loop_test() ->
    {ok, A} = start(),
    ok = add_subscriber(A, A, fun(_) -> true end),
    publish(A, "recurring"),
    timer:sleep(20), 
    ["recurring"] = messages(A).

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

reply_error(From) ->
    reply(From, error).

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.


