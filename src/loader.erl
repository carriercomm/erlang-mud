-module(loader).
-behaviour(gen_server).
-export([start_link/0, code_change/3, init/1, terminate/2]).
-export([handle_cast/2, handle_info/2]).

-include("object.hrl").

start_link() ->
    gen_server:start(loader, [], []).

init(_Args) ->
    ObjList = mnesia:activity(transaction, fun() -> mnesia:match_object(#object{_ = '_'}) end),
    gen_server:cast(self(), {load_list, ObjList}),
    {ok, []}.

handle_cast({load_list, []}, State) ->
    {noreply, State};
handle_cast({load_list, List}, State) ->
    [Obj|Tail] = List,
    object:load(Obj),
    gen_server:cast(self(), {load_list, Tail}),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("loader(): received handle_info('~p', _State)... wtf?~n", Info),
    State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
