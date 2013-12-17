-module(partial_ai).
-compile(export_all).
-record(pai_state,
        {hci    = none,
         npc    = none,
         mood   = none,
         target = none
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin_partial_ai() ->
    State = #pai_state{},
    loop(State).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(State) ->
    receive
        {_, _, attach_hci, HCI} ->
            loop(attach_hci(State, HCI));
        {_, _, attach_npc, NPC} ->
            loop(attach_npc(State, NPC))
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attach_hci(State, HCI) ->
    State#pai_state{hci = HCI}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attach_npc(State, NPC) ->
    util:rpc(NPC, {add_controller, self()}),
    util:rpc(NPC, {evict_other_controllers, self()}),
    State#pai_state{npc = NPC}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
