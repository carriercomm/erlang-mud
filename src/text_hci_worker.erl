-module(text_hci_worker).

-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, accept/2, post_accept/2, pre_terminate/2, handle_info/3, terminate/3]).

-include("text_hci.hrl").

start_link(InitialTextHCIData) ->
    gen_fsm:start_link(?MODULE, [InitialTextHCIData], []).

init(InitialTextHCIData) ->
    io:format("~p(~p): Acceptor spawned...~n", [?MODULE, self()]),
    gen_fsm:send_event(self(), do_accept),
    {ok, accept, InitialTextHCIData}.

accept(do_accept, [TextHCIData]) ->
    {Status, NewSock} = gen_tcp:accept(TextHCIData#text_hci_data.socket),
    supervisor:start_child(text_hci_sup, []),
    case {Status, NewSock} of
        {ok, NewSock} ->
            io:format("~p(~p): connection accepted~n", [?MODULE, self()]),
            NewTextHCIData = text_hci_mod_login:enter(TextHCIData#text_hci_data{socket = NewSock, current_mod = text_hci_mod_login}),
            {next_state, post_accept, NewTextHCIData};
        {error, NewSock} ->
            io:format("~p(~p): Failed to accept connection: ~p~n", [?MODULE, self(), NewSock]),
            {stop, normal, []}
    end.

%get_command({data, Msg}, TextHCIData) ->
%    [First|Others]= string:tokens(Msg, " \t"),
%    LookupTerm = string:to_lower(util:chomp(First)),
%    case trie:lookup_term(TextHCIData#text_hci_data.commandtrie, LookupTerm) of
%        undefined ->
%            gen_tcp:send(TextHCIData#text_hci_data.socket, "Unknown command, sorry!\n"),
%            {next_state, get_command, TextHCIData};
%        {Module, Function} ->
%            case apply(Module, Function, [Others, TextHCIData]) of
%                {ok, NewData} ->
%                    {next_state, get_command, NewData};
%                {change_state, NewTextHCIData, NewData} ->
%                    {next_state, NewTextHCIData, NewData};
%                stop ->
%                    {stop, normal, TextHCIData};
%                Other ->
%                    io:format("~p(~p): apply(~p, ~p, [..., ...]) returned: ~p~n", [?MODULE, self(), Module, Function, Other]),
%                    {stop, normal, TextHCIData}
%            end
%    end.

post_accept(Event, TextHCIData) ->
    io:format("~p(~p): post_accept(~p, ...): ignoring this event because I don't know how to handle it.\n", [?MODULE, self(), Event]),
    {next_state, post_accept, TextHCIData}.
    
pre_terminate(terminate, TextHCIData) ->
    % Do cleanup stuff here, like telling the actor it's been abandoned
    io:format("~p(~p): pre_terminate() firing for internal reason '~p'\n", [?MODULE, self(), TextHCIData#text_hci_data.mod_data]),
    {stop, normal, TextHCIData};
pre_terminate(_Ignored, TextHCIData) ->
    {next_state, pre_terminate, TextHCIData}.

handle_info({tcp, _Socket, Msg}, StateName, Data) ->
    CurrentMod = Data#text_hci_data.current_mod,
    case apply(CurrentMod, handle_input, [Msg, Data]) of
        {ok, NewData} ->
            {next_state, StateName, NewData};
        {newmod, NewMod, PreExitData} ->
            PreEnterData = apply(CurrentMod, leave, [PreExitData]),
            NewData = apply(NewMod, enter, [PreEnterData]),
            {next_state, StateName, NewData#text_hci_data{current_mod = NewMod}};
        {stop, Reason, NewData} ->
            gen_fsm:send_event(self(), terminate),
            {next_state, pre_terminate, NewData#text_hci_data{mod_data = Reason}}
    end;
handle_info({tcp_closed, _Socket}, StateName, Data) ->
    io:format("~p(~p): Client closed connection in state '~p'~n", [?MODULE, self(), StateName]),
    gen_fsm:send_event(self(), terminate),
    {next_state, pre_terminate, Data#text_hci_data{mod_data = connection_lost}}.

terminate(Reason, StateName, TextHCIData) ->
    io:format("~p(~p): Terminating in state '~p' for reason '~p', with state: ~p~n", [?MODULE, self(), StateName, Reason, TextHCIData]),
    ok.   


