-module(object).
-export([load/1, handle_cast/2, handle_call/3, terminate/2]).
-export([get_state/1, set_location/2, set_autoload/2]).
-export([new/1]).
-include("object.hrl").
%%% Contents of object.hrl:
%%% 
%% mnesia:create_table(object, 
%%   [
%%    {attributes, record_info(fields, object)}, 
%%    {index, [#object.location_x, #object.location_y, #object.location_z]}
%%   ]).
%% mnesia:change_table_copy_type(object, node(), disc_copies).
%
%-record(object,
%            {
%             uniqueID,
%             autoLoad,
%             startup,
%             subclass,
%             location_x,
%             location_y,
%             location_z
%            }
%).


load(Object) ->
    io:format("object:loader(): ...~n"),
    {Module, Function} = Object#object.startup,
    UniqueID = Object#object.uniqueID,
    Childspec = {
                    UniqueID,
                    {Module, Function, [UniqueID]},
                    permanent,
                    brutal_kill,
                    worker,
                    [Module]
                },
    supervisor:start_child(object_sup, Childspec).

new(Object) ->
    Trans = fun() ->
        NextID = list_to_binary(integer_to_list(mnesia:table_info(object, size) + 1)),
        ObjectWithID = Object#object{uniqueID = NextID},
        ok = mnesia:write(ObjectWithID),
        ObjectWithID
      end,
    {atomic, ObjectWithID} = mnesia:transaction(Trans),
    load(ObjectWithID).

get_state(Pid) ->
    gen_server:call(Pid, get_state).
set_location(Pid, Location) ->
    gen_server:call(Pid, {set_location, Location}).
set_autoload(Pid, Value) ->
    gen_server:call(Pid, {set_autoload, Value}).

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({set_location, Location}, _From, State) ->
    {X, Y, Z} = Location,
    NewState = State#object{location_x = X, location_y = Y, location_z = Z},
    ok = mnesia:activity(transaction, fun() -> mnesia:write(NewState) end),
    {reply, {ok, set_location}, NewState};

handle_call({set_autoload, Value}, _From, State) ->
    NewState = State#object{autoLoad = Value},
    ok = mnesia:activity(transaction, fun() -> mnesia:write(NewState) end),
    {reply, {ok, set_autoload}, NewState}.


handle_cast(shutdown, State) ->
    {stop, normal, State}.

terminate(shutdown, State) ->
    ok = mnesia:activity(transaction, fun() -> mnesia:write(State) end),
    ok.
