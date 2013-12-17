-module(text_hci_mod_login).

-behaviour(text_hci_module).

-export([enter/1, leave/1, handle_input/2]).

-include("text_hci.hrl").

enter(TextHCIData) ->
    io:format("~p(~p): entering...~n", [?MODULE, self()]),
    gen_tcp:send(TextHCIData#text_hci_data.socket, "Login: "),
    TextHCIData#text_hci_data{mod_data = getlogin}.

handle_input(Input, TextHCIData) ->
    io:format("~p(~p): leaving...~n", [?MODULE, self()]),
    case TextHCIData#text_hci_data.mod_data of
        getlogin ->
            gen_tcp:send(TextHCIData#text_hci_data.socket, "Password: "),
            {ok, TextHCIData#text_hci_data{login = string:to_lower(util:chomp(Input)), mod_data = getpassword}};
        getpassword ->
            Password = util:chomp(Input),
            io:format("~p(~p): Someday I'll try authenticating ~p/~p here.\n", [?MODULE, self(), TextHCIData#text_hci_data.login, Password]),
            % Try authentication here
            Socket = TextHCIData#text_hci_data.socket,
            gen_tcp:send(Socket, "Login successful, but I don't know how to remember you again later. Sorry.\n"),
            {newmod, text_hci_mod_lobby, TextHCIData}
    end.

leave(TextHCIData) ->
    TextHCIData.
