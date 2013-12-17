-module(comms).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
master_loop(CommChannelsList) ->
    receive
        {From, Tag, {subscribe, ChannelName}} ->
            io:format("comms:master_loop(): Received a subscribe request for ~p~n", [ChannelName]),
            ChannelExists = sanity_channel_exists(CommChannelsList, ChannelName),
            if  ChannelExists == false ->
                    io:format("comms:master_loop(): Channel '~p' doesn't exist.~n", [ChannelName]),
                    From ! {self(), Tag, failure, channel_does_not_exist},
                    master_loop(CommChannelsList);
                true -> ok
            end,
            AlreadySubscribed = sanity_channel_subscribed(CommChannelsList, ChannelName, From),
            if  AlreadySubscribed == true ->
                    io:format("comms:master_loop(): ~p is already subscribed to channel " ++
                              "'~p'.~n", [From, ChannelName]),
                    From ! {self(), Tag, success, already_subscribed},
                    master_loop(CommChannelsList);
                true -> ok
            end,
            NewChannelsList = subscribe(CommChannelsList, [], ChannelName, From),
            From ! {self(), Tag, success, subscribed},
            master_loop(NewChannelsList);
        {From, Tag, {unsubscribe, all}} ->
            From ! {self(), Tag, success, null},
            master_loop(CommChannelsList);
        {From, Tag, {unsubscribe, ChannelName}} ->
            io:format("comms:master_loop(): Received an unsubscribe request for ~p~n", [ChannelName]),
            ChannelExists = sanity_channel_exists(CommChannelsList, ChannelName),
            if  ChannelExists == false ->
                    io:format("comms:master_loop(): Channel '~p' doesn't exist.~n", [ChannelName]),
                    From ! {self(), Tag, failure, channel_does_not_exist},
                    master_loop(CommChannelsList);
                true -> ok
            end,
            IsSubscribed = sanity_channel_subscribed(CommChannelsList, ChannelName, From),
            if IsSubscribed == false ->
                    io:format("comms:master_loop(): ~p isn't subscribed to channel " ++
                              "'~p'.~n", [From, ChannelName]),
                    From ! {self(), Tag, success, not_subscribed},
                    master_loop(CommChannelsList);
                true -> ok
            end,
            NewChannelsList = unsubscribe(CommChannelsList, [], ChannelName, From),
            From ! {self(), Tag, success, unsubscribed},
            master_loop(NewChannelsList);
        {From, Tag, {broadcast, ChatName, ChannelName, Content}} ->
            io:format("comms:master_loop(): Received a broadcast request for ~p~n", [ChannelName]),
            ChannelExists = sanity_channel_exists(CommChannelsList, ChannelName),
            if  ChannelExists == false ->
                    io:format("comms:master_loop(): Channel '~p' doesn't exist.~n", [ChannelName]),
                    From ! {self(), Tag, failure, channel_does_not_exist},
                    master_loop(CommChannelsList);
                true -> ok
            end,
            IsSubscribed = sanity_channel_subscribed(CommChannelsList, ChannelName, From),
            if IsSubscribed == false ->
                    io:format("comms:master_loop(): ~p isn't subscribed to channel " ++
                              "'~p'.~n", [From, ChannelName]),
                    From ! {self(), Tag, failure, not_subscribed},
                    master_loop(CommChannelsList);
                true -> ok
            end,
            {ChannelName, Members, Handler} = lists:keyfind(ChannelName, 1, CommChannelsList),
            Handler(Members, ChatName, Content),
            From ! {self(), Tag, success, null},
            master_loop(CommChannelsList);
        {From, Tag, {list_channels}} ->
            From ! {self(), Tag, success, null},
            master_loop(CommChannelsList);
        {From, Tag, _} ->
            From ! {self(), Tag, failure, bad_comms_call},
            master_loop(CommChannelsList);
        _ ->
            % FIXME should really log a complaint here
            master_loop(CommChannelsList)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sanity_channel_exists(CommChannelsList, ChannelName) ->
%     io:format("comms:sanity_channel_exists(): <...>~n"),
    Status = lists:keyfind(ChannelName, 1, CommChannelsList),
%     io:format("comms:sanity_channel_exists(): Status is ~p~n", [Status]),
    Status.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sanity_channel_subscribed(CommChannelsList, ChannelName, From) ->
%     io:format("comms:sanity_channel_subscribed(): <...>~n"),
    {_, Members, _} = lists:keyfind(ChannelName, 1, CommChannelsList),
    lists:member(From, Members).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subscribe(ChannelsList, AlreadyVisitedList, DesiredChannelName, Subscriber) ->
    [{ThisChannelName, MembersList, Handler}| Remainder] = ChannelsList,
    case ThisChannelName of
        DesiredChannelName ->
            NewMembersList = [Subscriber] ++ MembersList,
            FinalList = [{DesiredChannelName, NewMembersList, Handler}] ++
                        Remainder ++
                        AlreadyVisitedList,
            FinalList;
        TheWrongChannelName ->
            subscribe(  Remainder,
                        [{TheWrongChannelName, MembersList, Handler}] ++ AlreadyVisitedList,
                        DesiredChannelName,
                        Subscriber)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unsubscribe(ChannelsList, AlreadyVisitedList, UnwantedChannelName, Subscriber) ->
    [{ThisChannelName, MembersList, Handler}| Remainder] = ChannelsList,
    case ThisChannelName of
        UnwantedChannelName ->
            NewMembersList = MembersList -- [Subscriber],
            FinalList = [{UnwantedChannelName, NewMembersList, Handler}] ++
                        Remainder ++
                        AlreadyVisitedList,
            FinalList;
        TheWrongChannelName ->
            unsubscribe(Remainder,
                        [{TheWrongChannelName, MembersList, Handler}] ++ AlreadyVisitedList,
                        UnwantedChannelName,
                        Subscriber)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
channel_chat(ChannelMembers, ChatName, Content) ->
    io:format("comms:channel_chat(): I am broadcasting '~p'...~n", [Content]),
    F = fun(Pid) ->
        io:format("comms:channel_chat(): to Pid ~p~n", [Pid]),
        Pid ! {comms, chat, ChatName, Content}
    end,
    lists:foreach(F, ChannelMembers).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_comms_channels() ->
    ChannelsList = [{auction, [], fun channel_chat/3},
                    {chat, [], fun channel_chat/3}],
    ChannelsList.