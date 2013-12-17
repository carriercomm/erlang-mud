-module(actor).

-behaviour(gen_server).

%% API
-export([start_link/1, init/1]).
-export([get_state/1, set_location/2, set_autoload/2]).
-export([get_display_name/1, set_display_name/2]).
-export([get_hit_regions/1, get_hit_region/2]).
-export([get_equipment/1]).
-export([format_hit_regions/1]).
-export([format_equipment/1]).
-export([new/0]).

%% Gen_server callbacks
-export([handle_call/3]).

%% Records
-include("object.hrl").
%%% Contents of object.hrl:
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
-record(actor, {
                display_name,
                hit_regions = [],
                equipment = []
        }).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(UniqueID) ->
    gen_server:start_link(actor, UniqueID, []).

init(UniqueID) ->
    [State] = mnesia:activity(transaction, fun() -> mnesia:read({object, UniqueID}) end),
    case is_record(State, object) of
        true ->
            case is_record(State#object.subclass, actor) of
                true ->
                    {ok, State};
                false ->
                    {error, 'not an actor object', State}
            end;
        false ->
            {error, 'not an object record', State}
    end.

new() ->
    object:new(humanoid_schema()).

get_state(Pid) ->
    object:get_state(Pid).
set_location(Pid, Location) ->
    object:set_location(Pid, Location).
set_autoload(Pid, Value) ->
    object:set_autoload(Pid, Value).

get_display_name(Pid) ->
    gen_server:call(Pid, get_display_name).
set_display_name(Pid, Name) ->
    gen_server:call(Pid, {set_display_name, Name}).
get_hit_regions(Pid) ->
    gen_server:call(Pid, get_hit_regions).
get_hit_region(Pid, Region) ->
    gen_server:call(Pid, {get_hit_region, Region}).
get_equipment(Pid) ->
    gen_server:call(Pid, get_equipment).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

handle_call(get_state, From, State) ->
    object:handle_call(get_state, From, State);
handle_call({set_location, Location}, From, State) ->
    object:handle_call({set_location, Location}, From, State);
handle_call({set_autoload, Value}, From, State) ->
    object:handle_call({set_autoload, Value}, From, State);

handle_call(get_display_name, _From, State) ->
    ActorState = State#object.subclass,
    DisplayName = ActorState#actor.display_name,
    {reply, DisplayName, State};

handle_call({set_display_name, Name}, _From, State) ->
    OldActorState = State#object.subclass,
    NewActorState = OldActorState#actor{display_name = Name},
    NewState = State#object{subclass = NewActorState},
    ok = mnesia:activity(transaction, fun() -> mnesia:write(NewState) end),
    {reply, {ok, set_display_name}, NewState};

handle_call(get_hit_regions, _From, State) ->
    ActorState = State#object.subclass,
    {reply, ActorState#actor.hit_regions, State};

handle_call({get_hit_region, Region}, _From, State) ->
    HitRegions = State#object.subclass#actor.hit_regions,
    case lists:keyfind(Region, 1, HitRegions) of
        false ->
            {reply, {error, 'no such region'}, State};
        Tuple ->
            {reply, Tuple, State}
    end;

handle_call(get_equipment, _From, State) ->
    ActorState = State#object.subclass,
    {reply, ActorState#actor.equipment, State}.

%% ===================================================================
%% Miscellaneous internal
%% ===================================================================

humanoid_schema() ->
    #object{startup = {?MODULE, start_link},
            subclass = #actor{
                               hit_regions = [{high, [{head, 100}]},
                                              {mid, [{rarm, 100},
                                                     {body, 100},
                                                     {larm, 100}
                                                    ]
                                              },
                                              {low, [{rleg, 100},
                                                     {lleg, 100}
                                                    ]
                                              }
                                             ],
                               equipment = [{high, [{head, []}
                                                   ]
                                            },
                                            {mid, [{rarm, []},
                                                   {body, []},
                                                   {larm, []}
                                                  ]
                                            },
                                            {low, [{rleg, []},
                                                   {lleg, []}
                                                  ]
                                            }
                                           ]
                             }
           }.

format_hit_regions(Regions) ->
    format_hit_regions(Regions, []).
format_hit_regions([], Chars) ->
    Chars;
format_hit_regions(Regions, Chars) ->   
    [R|T] = Regions,
    {Name, Limbs} = R,
    Summary = io_lib:format("~s:\t\t ~s~n", [atom_to_list(Name), format_limbs(Limbs)]),
    format_hit_regions(T, Chars ++ Summary).
       
format_limbs(Limbs) ->
    format_limbs(Limbs, []).
format_limbs([], Chars) ->
    Chars;
format_limbs(Limbs, Chars) ->
    [{Name, HP}|T] = Limbs,
    Summary = io_lib:format("~s: ~p\t", [atom_to_list(Name), HP]),
    format_limbs(T, Chars ++ Summary).

format_equipment(Regions) ->
    format_equipment(Regions, []).
format_equipment([], Chars) ->
    Chars;
format_equipment(Regions, Chars) ->
    [R|T] = Regions,
    {Name, Limbs} = R,
    Summary = io_lib:format("~s:\t\t ~s~n", [atom_to_list(Name), format_limbs_eq(Limbs)]),
    format_equipment(T, Chars ++ Summary).

format_limbs_eq([]) ->
    "no limbs remain";
format_limbs_eq(Limbs) ->
    format_limbs_eq(Limbs, []).
format_limbs_eq([], Chars) ->
    Chars;
format_limbs_eq(Limbs, Chars) ->
    [{Name, Items}|T] = Limbs,
    Summary = io_lib:format("~s: ~s\t", [atom_to_list(Name), format_limb_eq(Items)]),
    format_limbs_eq(T, Chars ++ Summary).

format_limb_eq([]) ->
    "nothing!";
format_limb_eq(Items) ->
    format_limb_eq(Items, []).
format_limb_eq([], Chars) ->
    Chars;
format_limb_eq(Items, Chars) ->
    [I|T] = Items,
    format_limb_eq(T, usable:get_display_name(I) ++ Chars).

