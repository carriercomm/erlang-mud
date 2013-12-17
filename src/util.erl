-module(util).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sleep(Time) ->
    receive
    after Time -> true
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
timeout_event(Time, Event) ->
    receive
    after Time -> Event()
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
callback_timeout(CallerPid, AlarmId, Timeout) ->
    spawn(fun() ->
            receive
            after Timeout ->
                CallerPid ! AlarmId
            end
          end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
periodic_timer(CallerPid, Time) ->
    Callback = fun() -> CallerPid ! periodic end,
    timeout_event(Time, Callback),
    periodic_timer(CallerPid, Time).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rpc(Pid, Message) ->
    rpc(Pid, Message, 1000).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_tag() ->
    random:uniform(2147483648).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rpc(Pid, Message, Timeout) ->
    Tag = gen_tag(),
    Pid ! {self(), Tag, Message},
    receive
        {Pid, Tag, Status, Extra} ->
%             io:format("RPC operation completed.~n"),
            {Status, Extra}
%         Other ->
%             io:format("RPC operation received some other message, dumping: ~p~n", [Other]),
%             {failure, unknown}
    after Timeout ->
        io:format("RPC operation timed out.~n"),
        {failure, unknown}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
toupper_first_char(String) when (String == []) ->
    String;
toupper_first_char(String) ->
    [FirstChar|Rest] = String,
    [string:to_upper(FirstChar)] ++ [Rest].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rstrip(String, CharList) ->
    OperatingString = lists:reverse(String),
    RegExp = "[" ++ CharList ++ "]",
    lists:reverse(rstrip(OperatingString, RegExp, inner)).
rstrip(String, _RegExp, inner) when (String == [])->
    [];
rstrip(String, RegExp, inner) ->
    [Headchar|Rest] = String,
    Status = re:run([Headchar], RegExp),
    case Status of
        {match, _} ->
            rstrip(Rest, RegExp, inner);
        nomatch ->
            String
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
chomp(String) ->
    rstrip(String, "\r\n\t ").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lstrip(String) when (String == []) ->
    String;
lstrip(String) ->
    lstrip(String, "\t ").
lstrip(String, CharList) ->
    RegExp = "[" ++ CharList ++ "]",
    rstrip(String, RegExp, inner).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filter_list(List, _Remove) when (List == []) ->
    [];
filter_list(List, Remove) ->
    [Head|Rest] = List,
    case Head of
        Remove ->
            filter_list(Rest, Remove);
        _ ->
            [Head] ++ filter_list(Rest, Remove)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
broadcast(_What, List) when (List == []) ->
    ok;
broadcast(What, List) ->
    [Next|Rest] = List,
    Next ! What,
    broadcast(What, Rest).