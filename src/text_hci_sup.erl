-module(text_hci_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-include("text_hci.hrl").

start_link(Port) ->
    process_flag(trap_exit, true),
    {ok, Listen} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
    InitialStateData = #text_hci_data{socket = Listen},
    Return = supervisor:start_link({local, ?MODULE}, ?MODULE, InitialStateData),
    {ok, _Pid} = supervisor:start_child(text_hci_sup, []),
    Return.

init(InitialStateData) ->
    {ok, { {simple_one_for_one, 2, 10},
           [
            {ignored_child_id,
             {text_hci_worker, start_link, [InitialStateData]},
             temporary, % if they die, the connection is severed anyway
             brutal_kill,
             worker,
             [text_hci]
            }
           ]
         }
    }.

