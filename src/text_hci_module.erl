-module(text_hci_module).

-callback enter(TextHCIData :: tuple()) -> NewTextHCIData :: tuple().
-callback leave(TextHCIData :: tuple()) -> NewTextHCIData :: tuple().
-callback handle_input(Input :: nonempty_string(), TextHCIData :: tuple()) -> 
    {ok, NewTextHCIData :: tuple()} |
    {newmod, NewMod :: atom(), NewTextHCIData :: tuple()} |
    {stop, Reason :: atom(), NewTextHCIData :: tuple()}.
-callback handle_other(Other :: term(), TextHCIData :: tuple()) ->
    {ok, NewTextHCIData :: tuple()} |
    {newmod, NewMod :: atom(), NewTextHCIData :: tuple()} |
    {stop, Reason :: atom(), NewTextHCIData :: tuple()}.

