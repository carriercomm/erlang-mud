-module(hci).
-compile(export_all).
-record(hci_state,
        {   pai = none,
            credentials = none,
            chat_name = none
        }).

-define(ANSI_RESET,     [27,91,$0,109]).
-define(ANSI_BRIGHT,    [27,91,$1,109]).
-define(ANSI_DIM,       [27,91,$2,109]).
-define(ANSI_UNDRSC,    [27,91,$4,109]).
-define(ANSI_BLINK,     [27,91,$5,109]).
-define(ANSI_REVERSE,   [27,91,$7,109]).
-define(ANSI_HIDDEN,    [27,91,$8,109]).
-define(ANSI_FG_BLK,    [27,91,$3,$0,109]).
-define(ANSI_FG_RED,    [27,91,$3,$1,109]).
-define(ANSI_FG_GRN,    [27,91,$3,$2,109]).
-define(ANSI_FG_YEL,    [27,91,$3,$3,109]).
-define(ANSI_FG_BLU,    [27,91,$3,$4,109]).
-define(ANSI_FG_MAG,    [27,91,$3,$5,109]).
-define(ANSI_FG_CYA,    [27,91,$3,$6,109]).
-define(ANSI_FG_WHI,    [27,91,$3,$7,109]).
-define(ANSI_BG_BLK,    [27,91,$4,$0,109]).
-define(ANSI_BG_RED,    [27,91,$4,$1,109]).
-define(ANSI_BG_GRN,    [27,91,$4,$2,109]).
-define(ANSI_BG_YEL,    [27,91,$4,$3,109]).
-define(ANSI_BG_BLU,    [27,91,$4,$4,109]).
-define(ANSI_BG_MAG,    [27,91,$4,$5,109]).
-define(ANSI_BG_CYA,    [27,91,$4,$6,109]).
-define(ANSI_BG_WHI,    [27,91,$4,$7,109]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command_look(Socket, State, _Args) ->
    % Check if we're in an attached state
    % If there are no arguments, we want a brief description of everything we see
        % RPC to the partial AI requesting descriptions of everything
    % If there *are* arguments, we want a deep description of something in particular
        % RPC to the partial AI requesting description of the first token/argument; ignore the rest
        % FIXME be sure to *cull* the rest as well...
    ResponseText = ?ANSI_BRIGHT ++ "You see... nothing!\r\n" ++ ?ANSI_RESET,
    gen_tcp:send(Socket, ResponseText),
    State.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command_say(Socket, State, Args) ->
    ResponseText = ?ANSI_FG_YEL ++ "You say, '" ++
                    util:toupper_first_char(Args) ++
                    "'.\r\n" ++ ?ANSI_RESET,
    gen_tcp:send(Socket, ResponseText),
    State.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command_chat(Socket, State, Args) ->
    {Status, Extra} = util:rpc(comms_master, {broadcast, State#hci_state.chat_name, chat, Args}),
    case Status of
        failure ->
            case Extra of
                not_subscribed ->
                    ResponseText = ?ANSI_FG_RED ++ "You aren't subscribed to that channel.\r\n" ++
                                    ?ANSI_RESET;
                _ ->
                    io:format("hci:command_chat(): RPC call returned {~p,~p}~n", [Status, Extra]),
                    ResponseText = ?ANSI_FG_RED ++ "That's funny... something is broken.\r\n" ++
                                    ?ANSI_RESET
            end,
            gen_tcp:send(Socket, ResponseText);
        _ ->
            ok
    end,
    State.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command_attach(_Socket, State, _Args) ->
    State.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command_comms(Socket, State, Args) ->
    ChannelLen = string:cspan(Args, " \t"),
    Channel = string:to_lower(string:strip(string:substr(Args, 1, ChannelLen))),
    ChannelAtom = command_comms_channel_atom_from_name(Channel),
    ChanState = string:to_lower(string:strip(string:substr(Args, ChannelLen + 1))),
    case ChanState of
        "on" ->
            {Status, Extra} = util:rpc(comms_master, {subscribe, ChannelAtom}),
            case Status of
                failure ->
                    case Extra of
                        unknown ->
                            ResponseText = 
                                ?ANSI_BRIGHT ++ ?ANSI_FG_RED ++ "Failed to subscribe; " ++
                                "please contact an administrator, this may be serious.\r\n" ++
                                ?ANSI_RESET;
                        _ ->
                            ResponseText = 
                              ?ANSI_FG_RED ++ "Failed to subscribe; either this channel doesn't " ++
                              "exist or you're not allowed to subscribe to it.\r\n" ++ ?ANSI_RESET
                    end;
                _ ->
                    ResponseText = 
                        ?ANSI_FG_RED ++ "Channel " ++
                        ?ANSI_BRIGHT ++ ?ANSI_FG_WHI ++ Channel ++ ?ANSI_RESET ++
                        ?ANSI_FG_RED ++ " is now " ++ ?ANSI_RESET ++
                        ?ANSI_BRIGHT ++ ?ANSI_FG_WHI ++ "ON\r\n" ++ ?ANSI_RESET
            end;
        "off" ->
            {Status, Extra} = util:rpc(comms_master, {unsubscribe, ChannelAtom}),
            case Status of
                failure ->
                    case Extra of
                        unknown ->
                            ResponseText = 
                                ?ANSI_BRIGHT ++ ?ANSI_FG_RED ++ "Failed to unsubscribe; " ++
                                "please contact an administrator, this may be serious.\r\n" ++
                                ?ANSI_RESET;
                        _ ->
                            ResponseText = 
                                ?ANSI_FG_RED ++ "Failed to unsubscribe; either the channel doesn't" ++
                                " exist or you've been " ++ ?ANSI_BRIGHT ++ "forced "
                                ++ ?ANSI_RESET ++ ?ANSI_FG_RED ++
                                "to listen to it.\r\n" ++ ?ANSI_RESET
                    end;
                _ ->
                    ResponseText = 
                        ?ANSI_FG_RED ++ "Channel " ++
                        ?ANSI_BRIGHT ++ ?ANSI_FG_WHI ++ Channel ++ ?ANSI_RESET ++
                        ?ANSI_FG_RED ++ " is now " ++ ?ANSI_RESET ++
                        ?ANSI_BRIGHT ++ ?ANSI_FG_WHI ++ "OFF\r\n" ++ ?ANSI_RESET
            end;
        _ ->
            ResponseText = ?ANSI_BRIGHT ++ ?ANSI_FG_RED ++ "I didn't get that-- did you want "
                            "the channel ON or OFF?\r\n" ++ ?ANSI_RESET
    end,
    gen_tcp:send(Socket, ResponseText),
    State.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command_comms_channel_atom_from_name(Name) ->
    if
        Name == "auction" ->
            auction;
        Name == "chat" ->
            chat;
        true ->
            undefined
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setup_hci() ->
    CommandList = [ {"look",    fun command_look/3},
                    {"logout",  logout},
                    {"say",     fun command_say/3},
                    {"shutdown",shutdown},
                    {"comms",   fun command_comms/3},
                    {"chat",    fun command_chat/3},
                    {"attach",  fun command_attach/3}],
    CommandTrie = trie:new(),
    setup_hci_commandtrie(CommandTrie, CommandList),
    CommandTrie.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setup_hci_commandtrie(_, CommandList) when (CommandList == []) ->
    void;
setup_hci_commandtrie(Trie, CommandList) ->
    [{Command, Action}|Remainder] = CommandList,
%     io:format("trie:add_term(T, ~p, ~p)~n", [Command, Action]),
    trie:add_term(Trie, Command, Action),
    setup_hci_commandtrie(Trie, Remainder).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin_slave(Listen, CommandTrie) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    hci_slave_master ! {accept_pool, decrement},
    slave_loop(Socket, CommandTrie, #hci_state{}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
slave_loop(Socket, CommandTrie, State) when (State#hci_state.credentials == none) ->
    gen_tcp:send(Socket, ?ANSI_RESET ++ "Username: "),
    receive
        {tcp, Socket, PreUsername} ->
            ok
    end,
    Username = string:to_lower(util:chomp(PreUsername)),
    Chatname = util:toupper_first_char(Username),
    gen_tcp:send(Socket, "Password: " ++ ?ANSI_BG_BLK ++ ?ANSI_FG_BLK),
    receive
        {tcp, Socket, Password} ->
            ok
    end,
    {Status, Reason} = gen_server:call(control_service, {authn, {password, Username, Password}}),
    io:format("hci:slave_loop(): {Status, Reason} is {~p, ~p}~n", [Status, Reason]),
    case Status of
    _ ->
        gen_tcp:send(Socket, ?ANSI_RESET ++ "Welcome to the MUD, " ++ Chatname ++ ".\n"),
        slave_loop(Socket, CommandTrie,
            State#hci_state{credentials = {Username, nopass}, chat_name = Chatname})
%     error ->
%         gen_tcp:send(Socket, ?ANSI_RESET ++ Reason ++ "\n"),
%         slave_loop(Socket, CommandTrie, State)
    end;
slave_loop(Socket, CommandTrie, State) ->
    receive
        {tcp, Socket, Data} ->
            io:format("hci_loop(): Received: ~p~n", [Data]),
            UsefulData = util:chomp(Data),
            CommandLen = string:cspan(UsefulData, " \t"),
            Command = string:to_lower(lists:sublist(UsefulData, CommandLen)),
            case trie:lookup_term(CommandTrie, Command) of
                shutdown ->
                    hci_slave_master ! {accept_pool, decrement},
                    ok;
                logout ->
                    % FIXME we need to do cleanup here...
                    % Detach from partial AI if we're attached
                    % Unsubscribe from all channels
                    hci_slave_master ! {accept_pool, decrement},
                    ok;
                undefined ->
                    Response = ?ANSI_BRIGHT ++ ?ANSI_FG_RED ++
                                "Nigga you crazy, I ain't never HEARD of that!\r\n" ++
                                ?ANSI_RESET ++ ?ANSI_FG_RED ++
                                "(You probably entered an ambiguous command; try "
                                "typing it out fully.)\r\n" ++ ?ANSI_RESET,
                    gen_tcp:send(Socket, Response),
                    slave_loop(Socket, CommandTrie, State);
                CommandHandler ->
                    NewState = CommandHandler(  Socket,
                                                State,
                                                util:lstrip(lists:sublist(  UsefulData,
                                                                            CommandLen + 1,
                                                                            length(UsefulData) - CommandLen))),
                    slave_loop(Socket, CommandTrie, NewState)
            end;
        {tcp_closed, Socket} ->
            hci_slave_master ! {accept_pool, decrement},
            io:format("Socket closed, bailing.~n"),
            ok;
        {comms, ChannelName, FromName, Content} ->
            case ChannelName of
                chat ->
                    io:format("hci_loop(): rcvd {comms, chat, ~p, ~p}~n", [FromName, Content]),
                    String = ?ANSI_BRIGHT ++ ?ANSI_FG_YEL ++ FromName ++ " chats: " ++ Content ++
                             "\r\n" ++ ?ANSI_RESET;
                auction ->
                    io:format("hci_loop(): rcvd {comms, auction, ~p, ~p}~n", [FromName, Content]),
                    String = ?ANSI_BRIGHT ++ ?ANSI_FG_YEL ++ FromName ++ " auctions: " ++
                                Content ++ "\r\n" ++ ?ANSI_RESET
            end,
            gen_tcp:send(Socket, String),
            slave_loop(Socket, CommandTrie, State)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin_slave_master(Listen, CommandTrie) ->
    process_flag(trap_exit, true),
    slave_master(Listen, CommandTrie, 0).
slave_master(Listen, CommandTrie, AcceptPoolSize) when (AcceptPoolSize < 1) ->
    PID = spawn(fun() -> begin_slave(Listen, CommandTrie) end),
    link(PID),
    io:format("hci:slave_master(): spawned a slave process~n"),
    slave_master(Listen, CommandTrie, AcceptPoolSize + 1);
slave_master(Listen, CommandTrie, AcceptPoolSize) ->
    receive
        % When a slave process accepts an incoming connection, it will notify us so that we can
        %   spawn another to accept further connections.
        {accept_pool, decrement} ->
            io:format("hci_slave_master(): AcceptPoolSize decremented~n"),
            slave_master(Listen, CommandTrie, AcceptPoolSize - 1);
        % When a slave process dies unexpectedly, dump its stack trace to the console
        {'EXIT', Pid, Why} ->
            io:format("hci:slave_master(): ~p died with: ~p~n", [Pid, Why])
    end,
    slave_master(Listen, CommandTrie, AcceptPoolSize).
