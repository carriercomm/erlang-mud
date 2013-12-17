-module(text_hci_mod_attached).

-behaviour(text_hci_module).

-export([enter/1, leave/1, handle_input/2]).
-export([do_look/2, do_logout/2, do_lobby/2]).

-include("text_hci.hrl").

enter(TextHCIData) ->
    io:format("~p(~p): entering...~n", [?MODULE, self()]),
    case TextHCIData#text_hci_data.commandtrie of
        undefined ->
            TextHCIData#text_hci_data{commandtrie = setup_text_hci_commandtrie()};
        _Other ->
            TextHCIData
    end.

leave(TextHCIData) ->
    % Do stuff like letting the attached actor know that we're no longer in control
    io:format("~p(~p): leaving...~n", [?MODULE, self()]),
    TextHCIData.

handle_input("\r\n", Data) ->
    {ok, Data};
handle_input("\n", Data) ->
    {ok, Data};
handle_input(Input, Data) ->
    [First|Others] = string:tokens(util:chomp(Input), " \t"),
    case trie:lookup_term(Data#text_hci_data.commandtrie, string:to_lower(First)) of
        undefined ->
            gen_tcp:send(Data#text_hci_data.socket, "Unknown command, sorry!\n"),
            {ok, Data};
        {Module, Function} ->
            apply(Module, Function, [Others, Data])
    end.

setup_text_hci_commandtrie() ->
    CommandList = [ {"look",        {text_hci_mod_attached, do_look}},
                    {"lobby",       {text_hci_mod_attached, do_lobby}},
                    {"say",         {text_hci_mod_attached, do_say}},
                    {"comms",       {text_hci_mod_attached, do_comms}},
                    {"chat",        {text_hci_mod_attached, do_chat}}
                  ],
    Trie = trie:new(),
    setup_text_hci_commandtrie(Trie, CommandList).
setup_text_hci_commandtrie(Trie, []) ->
    Trie;
setup_text_hci_commandtrie(Trie, CommandList) ->
    [{Term, Action}|Rest] = CommandList,
    trie:add_term(Trie, Term, Action),
    setup_text_hci_commandtrie(Trie, Rest).

do_lobby(_Tokens, TextHCIData) ->
    {newmod, text_hci_mod_lobby, TextHCIData}.

do_look(_Tokens, StateData) ->
    gen_tcp:send(StateData#text_hci_data.socket, "You see a little pot type thing.\n"),
    {ok, StateData}.

do_logout(_Tokens, StateData) ->
    gen_tcp:close(StateData#text_hci_data.socket),
    {stop, logout, StateData}.

