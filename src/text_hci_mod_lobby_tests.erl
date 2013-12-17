-module(text_hci_mod_lobby_tests).
-include_lib("eunit/include/eunit.hrl").
-include("text_hci.hrl").

enter_test() ->
    code:add_path("meck/ebin"),
    meck:new(gen_tcp, [unstick]),
    meck:expect(gen_tcp, send, fun(_Socket, Msg) -> self() ! {tcp_sent, Msg}, ok end),
    ?assertEqual(#text_hci_data{mod_data = top}, text_hci_mod_lobby:enter(#text_hci_data{})),
               
    receive
        {tcp_sent, Msg} ->
            ?assertEqual("--You are in the lobby--\n" ++
                           "\n" ++
                           "Select an option:\n" ++
                           "  1) Create a character\n" ++
                           "  2) Take control of an existing character\n" ++
                           "  3) Logout\n",
                         Msg)
    end,
    meck:unload(gen_tcp).

handle_input_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        {inorder,
          [ fun top_input_1/1,
            fun top_input_2/1,
            fun top_input_3/1,
        %    fun top_input_newline_only/1,
        %    fun top_input_empty/1,
            fun create_input/1,
            fun attach_input/1
          ]
        }
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
    % We could catch the "Unrecognized input..." string here, but meh
    [?_assertEqual({ok, #text_hci_data{mod_data = top}}, NewlineStringResponse)].

top_input_empty(_) -> 
    EmptyStringResponse = text_hci_mod_lobby:handle_input("", #text_hci_data{mod_data = top}),
    % We could catch the "Unrecognized input..." string here, but meh
    [?_assertEqual({ok, #text_hci_data{mod_data = top}}, EmptyStringResponse)].

create_input(_) ->
    CreateInputResponse = text_hci_mod_lobby:handle_input("\r\n", #text_hci_data{mod_data = create}),
    receive {tcp_sent, CreateInputMsg} -> ok end,
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
    code:add_path("meck/ebin"),
    meck:new(gen_tcp, [unstick]),
    meck:expect(gen_tcp, send, fun(_Socket, Msg) -> self() ! {tcp_sent, Msg}, ok end),
    meck:expect(gen_tcp, close, fun(_Socket) -> ok end),
    ok.

cleanup(_Ignored) ->
    receive _Any ->
        cleanup(ok)
    after 0 ->
        true
    end,
    meck:unload(gen_tcp).   
