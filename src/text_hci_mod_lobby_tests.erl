-module(text_hci_mod_lobby_tests).
-include_lib("eunit/include/eunit.hrl").
-include("text_hci.hrl").

-define(tcptest(F), {setup, fun setup/0, fun cleanup/1, F}).

%-export([setup/0, cleanup/1, handle_input_test_/0]).

enter_top_test_() ->
    {"Happy path: entering state 'top' prints top level menu and returns expected output",
     ?tcptest(
       fun(_) ->
          EnterResponse = text_hci_mod_lobby:enter(#text_hci_data{}),
          receive {tcp_sent, Msg} -> ok end,
          [ ?_assertEqual(#text_hci_data{mod_data = top}, EnterResponse),
            ?_assertEqual("--You are in the lobby--\n" ++
                         "\n" ++
                         "Select an option:\n" ++
                         "  1) Create a character\n" ++
                         "  2) Take control of an existing character\n" ++
                         "  3) Logout\n",
                       Msg)
          ]
        end
      )
    }.

handle_input_test_() ->
    {inorder,
      [ {"Happy path: input of '1' in state 'top'", ?tcptest(fun top_input_1/1)},
        {"Happy path: input of '2' in state 'top'", ?tcptest(fun top_input_2/1)},
        {"Happy path: input of '3' in state 'top'", ?tcptest(fun top_input_3/1)},
        {"Happy path: input of '\\r\\n' in state 'top'", ?tcptest(fun top_input_newline_only/1)},
        {"Unhappy path: input of '' in state 'top'", ?tcptest(fun top_input_empty/1)},
        {"Happy path: input of '\\r\\n' in state 'create'", ?tcptest(fun create_input/1)},
        {"Happy path: input of '\\r\\n' in state 'attach'", ?tcptest(fun attach_input/1)}
      ]
    }.

top_input_1(_) ->
    Response1 = text_hci_mod_lobby:handle_input("1\r\n", #text_hci_data{mod_data = top}),
    receive {tcp_sent, Msg1} -> ok end,
    [
        ?_assertEqual({ok, #text_hci_data{mod_data = create}}, Response1),
        ?_assertEqual("--Character creation menu--\n" ++
                        "\n" ++
                        "... ok there's actually nothing to do here, just send me some "
                        "input and I'll drop you back you to the lobby menu.\n",
                      Msg1)
    ].

top_input_2(_) ->
    Response2 = text_hci_mod_lobby:handle_input("2\r\n", #text_hci_data{mod_data = top}),
    receive {tcp_sent, Msg2} -> ok end,
    [ ?_assertEqual({ok, #text_hci_data{mod_data = attach}}, Response2),
      ?_assertEqual("Hit any key, and I'll drop you into the get_commands() state...\n", Msg2) ].

top_input_3(_) ->
    Response3 = text_hci_mod_lobby:handle_input("3\r\n", #text_hci_data{mod_data = top}),
    [?_assertEqual({stop, logout, #text_hci_data{mod_data = top}}, Response3)].

top_input_newline_only(_) ->
    NewlineStringResponse = text_hci_mod_lobby:handle_input("\r\n", #text_hci_data{mod_data = top}),
    receive {tcp_sent, NewlineStringMsg} -> ok end,
    [ ?_assertEqual({ok, #text_hci_data{mod_data = top}}, NewlineStringResponse),
      ?_assertEqual(io_lib:format("Unrecognized input '~p', try again.\n", ["\r\n"]), NewlineStringMsg) ].

top_input_empty(_) -> 
    EmptyStringResponse = text_hci_mod_lobby:handle_input("", #text_hci_data{mod_data = top}),
    receive {tcp_sent, EmptyStringMsg} -> ok end,
    [ ?_assertEqual({ok, #text_hci_data{mod_data = top}}, EmptyStringResponse),
      ?_assertEqual(io_lib:format("Unrecognized input '~p', try again.\n", [""]), EmptyStringMsg) ].

create_input(_) ->
    CreateInputResponse = text_hci_mod_lobby:handle_input("\r\n", #text_hci_data{mod_data = create}),
    receive {tcp_sent, CreateInputMsg} -> ok end,
    % enter() gets called again here, temporarily, so there's actually a second tcp message to receive
    % ... but that's temporary, and we're already testing it in top_input_1/1, so I'm leaving it out
    [ ?_assertEqual({ok, #text_hci_data{mod_data = top}}, CreateInputResponse),
      ?_assertEqual("That's nice, dear! Let's go back to the lobby menu now.\n\n", CreateInputMsg)].

attach_input(_) ->
    AttachInputResponse = text_hci_mod_lobby:handle_input("\r\n", #text_hci_data{mod_data = attach}),
    receive {tcp_sent, AttachInputMsg} -> ok end,
    [
        ?_assertEqual({newmod, text_hci_mod_attached, #text_hci_data{mod_data = attach}}, AttachInputResponse),
        ?_assertEqual("WHOOOOOOSH! You're back in your body!\n", AttachInputMsg)
    ].

setup() ->
    code:add_path("../meck/ebin"),
    meck:new(gen_tcp, [unstick]),
    %meck:expect(gen_tcp, send, fun(_Socket, Msg) -> io:format("meck: passing along this msg: ~p~n", [Msg]), self() ! {tcp_sent, Msg}, ok end),
    meck:expect(gen_tcp, send, fun(_Socket, Msg) -> self() ! {tcp_sent, Msg}, ok end),
    meck:expect(gen_tcp, close, fun(_Socket) -> ok end),
    ok.

cleanup(_Ignored) ->
    receive _Any ->
        cleanup(ok)
    after 0 ->
        meck:unload(gen_tcp)
    end.
