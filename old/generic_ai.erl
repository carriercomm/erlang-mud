-module(generic_ai).
-compile([debug_info, export_all]).
-record(ai_state,
        {   target = none,
            timer_init = false,
            timer_pid = none,
            mode = acquire_target
        }
       ).

begin_generic_ai(Pid) ->
    State = #ai_state{},
    loop(Pid, State).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Pid, State) when (State#ai_state.timer_init == false) ->
    MyPid = self(),
    TimerPid = spawn(fun() -> util:periodic_timer(MyPid, 3000) end),
    loop(Pid, State#ai_state{ timer_init = true, timer_pid = TimerPid});
loop(Pid, State) when (State#ai_state.mode == dead) ->
    receive
        {broadcast, revived, Pid} ->
            io:format("generic_ai:loop(~p): Woohoo, back in action!!!~n", [self()]),
            NewState = #ai_state{timer_init = true, timer_pid = State#ai_state.timer_pid},
            loop(Pid, NewState);
        _ ->
            loop(Pid, State)
    end;
loop(Pid, State) ->
%     io:format("~p: begin_generic_ai(): loop!~n", [self()]),
    CurrentMode = State#ai_state.mode,
    CurrentTarget = State#ai_state.target, % hack because matching record attributes doesn't work in Erlang
    receive
        periodic ->
%             io:format("generic_ai:loop(~p): received a periodic event~n", [self()]),
            case CurrentMode of
                acquire_target ->
                    instruct_look(Pid),
                    loop(Pid, State);
                _ ->
                    loop(Pid, State)
            end;
        {Pid, _Tag, info_nearby_procs, List} ->
%             io:format("generic_ai:loop(~p): my actor got back to me with a list of nearby actors: ~p~n",
%                         [self(), List]),
            case CurrentMode of
                acquire_target ->
                    Target = choose_random_target(List),
                    case Target of
                        none_available ->
                            loop(Pid, State);
                        _ ->
                            io:format("generic_ai:loop(~p): Target acquired! (~p)~n", [self(), Target]),
                            NewState = State#ai_state{mode = kill, target = Target},
                            instruct_hit(Pid, Target),
                            loop(Pid, NewState)
                    end;
                _ ->
                    i_dont_care,
                    loop(Pid, State)
            end;
        {Pid, _Tag, hit_success, Victim} when (Victim == CurrentTarget) ->
            io:format("generic_ai:loop(~p): Successful hit on target!~n", [self()]),
            case CurrentMode of
                kill ->
                    instruct_hit(Pid, State#ai_state.target);
                _ ->
                    do_nothing
            end,
            loop(Pid, State);
        {Pid, _Tag, hit_success, _Victim} ->
            case CurrentMode of
                acquire_target ->
                    io:format("generic_ai:loop(~p): Successfully abused the body of my last target!~n", [self()]);
                _ ->
                    io:format("generic_ai:loop(~p): Thoroughly confused about a mis-hit!~n", [self()])
            end,
            loop(Pid, State);
        {broadcast, death, WhoDied} ->
            case WhoDied of
                CurrentTarget ->
                    io:format("generic_ai:loop(~p): Target neutralized!~n", [self()]),
                    NewState = State#ai_state{target = none, mode = acquire_target},
                    loop(Pid, NewState);
                _ ->
                    % FIXME note companion deaths here!
                    loop(Pid, State)
            end;
        {broadcast, received_hit, {Victim, _Aggressor}} ->
            case Victim of
                Pid ->
                    % FIXME note aggressors here!
                    loop(Pid, State);
                _ ->
                    % FIXME note attacks on friends?!
                    loop(Pid, State)
            end;
        {broadcast, revived, _WhoRevived} ->
            % FIXME note companion revives here!
            loop(Pid, State);
        {Pid, _Tag, death} ->
            io:format("generic_ai:loop(~p): OH SNAP WE GOT KILLT!~n", [self()]),
            NewState = State#ai_state{mode = dead},
            RespawnTime = random:uniform(60000),
            util:callback_timeout(Pid, {self(), ignored, resurrect}, RespawnTime),
            loop(Pid, NewState);
        Any ->
            io:format("generic_ai:loop(~p): received something I don't recognize: ~p~n", [self(), Any]),
            io:format("---- for reference, my actor's PID is ~p~n", [Pid]),
            loop(Pid, State)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
instruct_look(Pid) ->
    Pid ! {self(), ignored, {action, look, surroundings}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
instruct_hit(Pid, Target) ->
    Pid ! {self(), ignored, {action, hit, Target}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choose_random_target(TargetList) when (TargetList == []) ->
    none_available;
choose_random_target(TargetList) ->
    TargetCandidate = lists:nth(random:uniform(length(TargetList)), TargetList),
    {_Status, Alive} = util:rpc(TargetCandidate, {get_attr, alive}),
    case Alive of
        true ->
            TargetCandidate;
        false ->
            io:format("choose_random_target(~p): that target's already dead, still looking...~n", [self()]),
            SmallerList = util:filter_list(TargetList, TargetCandidate),
            choose_random_target(SmallerList)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%