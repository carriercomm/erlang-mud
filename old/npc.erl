-module(npc).
-compile([debug_info, export_all]).
-record(npc_state,
        {listeners = [],
         controllers = [],
         body_sections = [],
         basic_description = "a game object with no description",
         stupid_hitpoints = 10,
         stupid_speed = 1000,
         stupid_power = 2,
         action_queue = [],
         action_pending = false,
         location_registered = false,
         alive = true
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin_npc() ->
    State = #npc_state{},
    spawn(fun() -> loop(State) end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% I have just been created and have no location--
%   Register my location with location_service and continue looping
loop(State) when (State#npc_state.location_registered == false) ->
    % FIXME handle error gracefully (probably just repeat the loop)
    gen_server:call(location_service, {new_proc, blah}),
    loop(State#npc_state{location_registered = true});
% I have no AI controller--
%   Get / link with an AI controller and continue looping
loop(State) when (State#npc_state.controllers == []) ->
    % FIXME handle error *more* gracefully-- what are the failure modes here?
    %   probably just repeat the loop until we meet with success
    Controller = gen_server:call(control_service, request_ai_controller),
    case Controller of
        error ->
            loop(State);
        _ ->
            link(Controller),
            NewState = State#npc_state{controllers = [Controller]},
            loop(NewState)
    end;
% I have actions pending, which haven't been initiated--
%   put an action in-flight and continue looping
loop(State) when (State#npc_state.action_pending == false) and
                 (State#npc_state.action_queue /= []) ->
    [NextAction|Rest] = State#npc_state.action_queue,
    NewState = add_timeout_action(NextAction, State),
    FinalState = NewState#npc_state{action_queue = Rest},
    loop(FinalState);
% I have < 0 hitpoints--
%   broadcast my death and change state to 'dead' so that checks farther down will refuse to
%   perform zombie actions.
loop(State) when (State#npc_state.stupid_hitpoints =< 0) and
                 (State#npc_state.alive == true) ->
    gen_server:call(location_service, {broadcast, death, self()}),
    io:format("npc:loop(~p): I'm dead~n", [self()]),
    util:broadcast({self(), ignored, death}, State#npc_state.controllers),
    NewState = State#npc_state{alive = false},
    loop(NewState);
%     exit(death);

loop(State) ->
    % Pull State data into local variables, because matching on bare record values in receive()
    %   forces the use of unacceptably ugly Erlang syntax.
    ActionPending = State#npc_state.action_pending,
    ActionQueue = State#npc_state.action_queue,
    Alive = State#npc_state.alive,
    Controllers = State#npc_state.controllers,

    % Process input from the outside world! This is where the action happens.
    receive
        % Broadcasts are informative only; the ability to act on information is in the department
        %   of the AI / player, so we just forward it on to them
        {broadcast, Event, Args} ->
            % FIXME handle error gracefully
            util:broadcast({broadcast, Event, Args}, State#npc_state.controllers),
            loop(State);
        % Attribute inquiries will come in as RPC requests
        %   FIXME this should perhaps be filtered, i.e. not everyone should be allowed to see all
        %   of our attributes.
        {From, Tag, {get_attr, Attribute}} ->
            case Attribute of
                basic_description ->
                    Value = State#npc_state.basic_description;
                alive ->
                    Value = Alive;
                _ ->
                    Value = unknown_attribute
            end,
            From ! {self(), Tag, ok, Value},
            loop(State);
        % When we're hit by something, we're notified via message-- this is the only way for the
        %   hit to have any effect, since we are the sole owner of our state data.
        {From, _Tag, {get_hit, Power}} ->
            % FIXME handle error gracefully
            gen_server:call(location_service, {broadcast, received_hit, {self(), From}}),
            NewState = State#npc_state{stupid_hitpoints = State#npc_state.stupid_hitpoints - Power},
            loop(NewState);
        % An action request while dead will be ignored
        {_From, _Tag, {action, _Action, _Extra}} when (Alive == false) ->
            loop(State);
        % An action request while alive will be delayed for some time period before actual execution;
        %   only one action can be executed at a time, and if there is one already executing this
        %   new action will be added to a queue
        {From, Tag, {action, Action, Extra}} ->
            case ActionPending of
                % If there are no actions pending, throw this one into delayed-execution
                false ->
                    NewState = add_timeout_action({From,
                                                    Tag,
                                                    {alarmed_action, Action, Extra}},
                                                   State),
                    FinalState = NewState#npc_state{action_pending = true},
                    loop(FinalState);
                % If there're already actions pending, add this to the queue
                true ->
                    NewState = State#npc_state{action_queue = ActionQueue ++
                                                                [{From,
                                                                Tag,
                                                                {alarmed_action,
                                                                    Action,
                                                                    Extra}}]},
                    loop(NewState)
            end;
        % Actions having completed their delay stage while dead will be ignored
        {_From, _Tag, {alarmed_action, _Action, _Extra}} when (Alive == false) ->
            loop(State);
        % Actions having completed their delay stage while alive will be completed
        {From, Tag, {alarmed_action, Action, Extra}} ->
            case Action of
                look ->
                    % FIXME handle non-ok Status gracefully
                    {_Status, Value} = action_look(Extra),
                    From ! {self(), Tag, info_nearby_procs, Value};
                hit ->
                    ToBeHit = Extra,
                    action_hit(ToBeHit, State),
                    From ! {self(), Tag, hit_success, ToBeHit};
                _ ->
                    From ! {self(), Tag, error, unimplemented}
            end,
            NewState = State#npc_state{action_pending = false},
            loop(NewState);
        {_From, _Tag, resurrect} ->
            NewState = #npc_state{controllers = Controllers, location_registered = true},
            % FIXME handle error gracefully
            gen_server:call(location_service, {broadcast, revived, self()}),
            loop(NewState);
        Any ->
            io:format("npc:loop(): received this and I don't know what to do with it: ~p~n", [Any]),
            loop(State)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add a controller (provided to us by the control_service?) to the list of controllers we're
%   aware of.
%   FIXME The callback here isn't actually implemented on the control_service side, and I'm not
%       really sure what the purpose was to begin with...
% add_controller(State, Controller) ->
%     Status = gen_server:call(control_service, {authz, {take_control, Controller}}),
%     case Status of
%         ok ->
%             {ok, State#npc_state{controllers = State#npc_state.controllers ++ [Controller]}};
%         {error, Reason} ->
%             {error, Reason};
%         true ->
%             {error, unknown_return_from_controller_service}
%     end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action_look(surroundings) ->
    % FIXME handle error gracefully
    %   maybe just recurse until we succeed? it could be set-up as tail recursion...
    NearbyProcs = gen_server:call(location_service, {nearby_procs, blah}),
    NearbyProcsMinusMe = util:filter_list(NearbyProcs, self()),
    {ok, NearbyProcsMinusMe};
    %NearbyTerrain = gen_server:call(location, nearby_terrain),
    %NearbyObjs = gen_server:call(location, nearby_objs),
    %action_look_basic_list(NearbyTerrain ++ NearbyObjs, []).
%     {ok, ["a little clay pot", "a straw mat"]};
action_look(_) ->
    {error, dont_know_how_to_do_that_yet}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The *idea* of this function is fine, when the AI asks us to look at something in detail
% However, I'm not sure if the AI should be allowed to ask us to look at many things in a single
%   action.
% action_look_basic_list(ToBeLookedAt, Data) when (ToBeLookedAt == [] ) ->
%     Data;
% action_look_basic_list(ToBeLookedAt, OldData) ->
%     [Head|Tail] = ToBeLookedAt,
%     {Status, Description} = util:rpc(Head, {get_attr, basic_description}),
%     case Status of
%         ok ->
%             NewData = OldData ++ [Description];
%         error ->
%             NewData = OldData
%     end,
%     action_look_basic_list(Tail, NewData).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action_hit(ToBeHit, State) ->
    ToBeHit ! {self(), unimportant, {get_hit, State#npc_state.stupid_power}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_timeout_action(Action, State) ->
    util:callback_timeout(self(), Action, State#npc_state.stupid_speed),
    NewState = State#npc_state{action_pending = true},
    NewState.
