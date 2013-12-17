-module(mud).
-compile(export_all).

% mobile_init() ->
%     % This ends up being useful...
%     MyPid = self(),
%     % Seed the RNG, which we'll use to tag RPC calls
%     {A1,A2,A3} = now(),
%     random:seed(A1, A2, A3),
% 
%     put(willpower, 100),
%     put(rarm, true),
%     put(larm, false),
% 
%     mobile().

% mobile() ->
%     receive
%         alarm ->
%             io:format("Mobile[~p]: Received an alarm.~n", [self()]);
%         {From, Tag, ping} ->
%             io:format("Mobile[~p]: Received a ping.~n", [self()]),
%             From ! {self(), Tag, acknowledged};
%         {From, Tag, {get, Attribute}} ->
%             io:format("Mobile[~p]: Received an attribute-state inquiry.~n", [self()]),
%             From ! {self(), Tag, get(Attribute)};
%         Other ->
%             io:format("Mobile[~p]: Received something I don't know how to handle...~n", [self()])
%     end,
%     mobile().

% mobile_controller_init(MobilePid, Controller) ->
%     put(state, idle),
%     Controller(MobilePid).
% 
% generic_mobile_controller(MobilePid) ->
%     rpc(MobilePid, ping),
%     WillPower = rpc(MobilePid, {get, willpower}),
%     io:format("Controller: my willpower is currently ~p~n", [WillPower]),
%     sleep(5000),
%     generic_mobile_controller(MobilePid).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main(_) ->
    io:format("Running!~n"),
    random:seed(now()),
%     Mob = spawn(fun() -> generic_mobile() end),
    {ok, Listen} = gen_tcp:listen(8081, [{reuseaddr, true}, {active, true}]),
    CommandTrie = hci:setup_hci(),
    CommChannelsList = comms:init_comms_channels(),
    register(comms_master, spawn(fun() -> comms:master_loop(CommChannelsList) end)),
    % Bring the control service online; this is needed for authn/authz of incoming connections
    control_service:start_link(),
    % Bring the location service online; this is needed for the gameworld to do anything useful
    location_service:start_link(),
    % Bring the text-based HCI (telnet) service online
    HCISlaveMaster = spawn(fun()-> hci:begin_slave_master(Listen, CommandTrie) end),
    register(hci_slave_master, HCISlaveMaster),
    npc:begin_npc(),
    npc:begin_npc(),
    npc:begin_npc(),
    npc:begin_npc(),
    npc:begin_npc(),
    receive
        {never_arrives} ->
            ok
    end,
    gen_tcp:close(Listen).
