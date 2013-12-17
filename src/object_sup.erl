-module(object_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([init/1]).
-export([handle_cast/2]).

start_link() ->
    supervisor:start_link({local, object_sup}, object_sup, []).

init(_Args) ->
    {ok, {
            {one_for_one, 10, 10},
            [] % children will be dynamically added
    }}.

stop() ->
    gen_server:cast(object_sup, stop).

handle_cast(stop, State) ->
    io:format("object_sup:handle_cast(stop, ~p): ...~n", State),
    {stop, normal, State};
handle_cast(Other, State) ->
    io:format("object_sup:handle_cast(~p, <State>): unexpected cast!~n", Other),
    {noreply, State}.

