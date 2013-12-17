-module(text_hci_mod_lobby).

-behaviour(text_hci_module).

-export([enter/1, leave/1, handle_input/2]).

-include("text_hci.hrl").

handle_input(Input, TextHCIData) ->
    case TextHCIData#text_hci_data.mod_data of
        top ->
            handle_top_input(Input, TextHCIData);
        create ->
            handle_create_input(Input, TextHCIData);
        attach ->
            handle_attach_input(Input, TextHCIData);
        _Other ->
            io:format("~p(~p): text_hci_mod_lobby:handle_input/2: Unrecognized menu-state!!!\n", [?MODULE, self()]),
            {ok, enter(TextHCIData)}
    end.

enter(TextHCIData) ->
    io:format("~p(~p): entering...~n", [?MODULE, self()]),
    Message = 
        "--You are in the lobby--\n" ++
        "\n" ++
        "Select an option:\n" ++
        "  1) Create a character\n" ++
        "  2) Take control of an existing character\n" ++
        "  3) Logout\n",
    gen_tcp:send(TextHCIData#text_hci_data.socket, Message),
    TextHCIData#text_hci_data{mod_data = top}.

leave(TextHCIData) ->
    io:format("~p(~p): leaving...~n", [?MODULE, self()]),
    TextHCIData.

handle_top_input(Input, TextHCIData) ->
    Term = string:to_lower(util:chomp(Input)),
    case Term of
        "1" ->
            NewHCIData = enter_create_menu(TextHCIData),
            {ok, NewHCIData};
        "2" ->
            NewHCIData = enter_attach_menu(TextHCIData),
            {ok, NewHCIData};
        "3" ->
            gen_tcp:close(TextHCIData#text_hci_data.socket),
            {stop, logout, TextHCIData};
        _Other ->
            gen_tcp:send(TextHCIData#text_hci_data.socket,
                         io_lib:format("Unrecognized input '~p', try again.\n", [Input])),
            {ok, TextHCIData}
    end.

enter_create_menu(TextHCIData) ->
    Message = io_lib:format(
        "--Character creation menu--\n" ++
        "\n" ++
        "... ok there's actually nothing to do here, just send me some input and I'll drop you back you to the lobby menu.\n", []),
    gen_tcp:send(TextHCIData#text_hci_data.socket, Message),
    TextHCIData#text_hci_data{mod_data = create}.

handle_create_input(_Input, TextHCIData) ->
    gen_tcp:send(TextHCIData#text_hci_data.socket, "That's nice, dear! Let's go back to the lobby menu now.\n\n"),
    {ok, enter(TextHCIData)}.

enter_attach_menu(TextHCIData) ->
    Message = "Hit any key, and I'll drop you into the get_commands() state...\n",
    gen_tcp:send(TextHCIData#text_hci_data.socket, Message),
    TextHCIData#text_hci_data{mod_data = attach}.

handle_attach_input(_Input, TextHCIData) ->
    gen_tcp:send(TextHCIData#text_hci_data.socket, "WHOOOOOOSH! You're back in your body!\n"),
    {newmod, text_hci_mod_attached, TextHCIData}.

