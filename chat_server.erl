-module(chat_server).
-export([start/3, get_history/1, get_connected_clients/1, send_message_to_all_clients/2, make_admin/2]).

-spec start(atom(), pos_integer(), pos_integer()) -> ok.

start(ServerName, MaxClients, MaxHistoryCount) ->
  Clients = #{},
  MsgHistory = [],
  Topic = "",
  AdminOnlyTopic = false,
  ServerPid = spawn(fun() -> loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) end),
  io:format("Server Started with PID ~p~n",[ServerPid]),
  global:register_name(ServerName,ServerPid).

-spec loop(atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    receive
        {ClientName, ClientPid, connect_client} ->
            connect(ClientName, ClientPid, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {ClientName, Message, send_message} ->
            send_msg_client(ClientName, Message, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {Sender, Receiver, Message, send_private_message} ->
            send_private_msg_client(Sender, Receiver, Message, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {get_connected_clients} ->
            connected_clients(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {ClientName, get_connected_clients_client} ->
            connected_clients_client(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {history} ->
            msg_history(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {ClientName, history_client} ->
            msg_history_client(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {ClientName, EnableAdminOnly, updt_admin_only_topic} ->
            update_admin_only_topic(ClientName, EnableAdminOnly, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {ClientName, NewTopic, update_topic} ->
            update_topic_client(ClientName, NewTopic, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {ClientName, get_current_topic} ->
            get_topic_client(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {Message, send_message_to_all_clients} ->
            send_msg_server_to_all(Message, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {Message, ClientName, send_message_to_all_clients_except_given} -> 
            send_msg_server_to_all_except_given(Message, ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {ClientName, make_admin_server} ->
            make_admin_server(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {ClientName, get_admins} ->
            get_admins_client(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {ClientName, Status, update_status} ->
            update_client_status(ClientName, Status, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {AdminName, ClientName, kick_client} ->
            kick(AdminName, ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {AdminName, ClientName, Time, mute_client} ->
            mute(AdminName, ClientName, Time, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {AdminName, ClientName, unmute_client} ->
            unmute(AdminName, ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);

        {AdminName, ClientName, make_admin_client} ->
            make_admin_client(AdminName, ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
            
        {ClientName, disconnect_client} ->
            disconnect(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec connect(atom(), pid(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

connect(ClientName, ClientPid, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    CurrentClients = map_size(Clients),
    IsClient = maps:is_key(ClientName, Clients),
    if IsClient == true ->
        global:send(ClientName,{ServerName, "Client Name already been used. Cannot Establish Connection to the ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
    true -> 
        if CurrentClients >= MaxClients ->
            global:send(ClientName,{ServerName, "Client Limit Exceeded. Cannot Establish Connection to the ", connection_failed}),
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
        true ->
            Client = #{clientPid => ClientPid, isAdmin => false, isMuted => false, mutedTill => 0, status => online, personalHistory => []},
            Temp = maps:put(ClientName, Client, Clients),
            io:format("\n[" ++ get_time() ++ "] New Client Added To Group:~p~n", [ClientName]),
            Message = "New Client Added To Group: " ++ atom_to_list(ClientName),
            global:send(ServerName, {Message, ClientName, send_message_to_all_clients_except_given}),  
            global:send(ClientName,{ServerName, MsgHistory, MaxHistoryCount, latest_n_history}),          
            loop(ServerName, Temp, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
        end
    end.

-spec send_msg_client(atom(), string(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> ok | no_return().

send_msg_client(ClientName, Message, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsClient = maps:is_key(ClientName, Clients), 
    Msg = "[" ++ atom_to_list(ClientName) ++ "]: " ++ Message, 
    if IsClient == true ->
        Client = maps:get(ClientName, Clients),
        IsMutedClient = maps:get(isMuted, Client),
        if IsMutedClient == true-> 
            MuteEndTime = maps:get(mutedTill, Client),
            CurrentTime = calendar:local_time(),
            if CurrentTime >= MuteEndTime ->
                TempClient = maps:put(isMuted, false, Client),
                TempMap = maps:put(ClientName, TempClient, Clients),
                io:format("\n[" ++ get_time() ++ "] ~p: ~p~n", [ClientName, Message]),
                History = {"[" ++ get_time() ++ "]", ClientName, Message},
                TempHistory = lists:append(MsgHistory, [History]),
                global:send(ServerName, {Msg, ClientName, send_message_to_all_clients_except_given}),  
                loop(ServerName, TempMap, MaxClients, MaxHistoryCount, TempHistory, Topic, AdminOnlyTopic);
            true ->
                {{Year,Month,Day},{Hour,Min,Sec}} = MuteEndTime,
                MuteTime = lists:concat([Year,'/',Month,'/',Day,' ',Hour,':',Min,':',Sec]),
                global:send(ClientName,{ServerName, "You are Muted until "++ MuteTime ++ " Cannot Send Message to the ", connection_failed}),
                loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
            end;
        true ->
            io:format("\n[" ++ get_time() ++ "] ~p: ~p~n", [ClientName, Message]),
            History = {"[" ++ get_time() ++ "]", ClientName, Message},
            TempHistory = lists:append(MsgHistory, [History]),
            global:send(ServerName, {Msg, ClientName, send_message_to_all_clients_except_given}),  
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, TempHistory, Topic, AdminOnlyTopic)
        end;
    true ->
        global:send(ClientName, {ServerName, "Client Name not found. Cannot Send Message to Server ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec send_private_msg_client(atom(), atom(), string(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

send_private_msg_client(Sender, Receiver, Message, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsClientSender = maps:is_key(Sender, Clients),
    IsClientReceiver = maps:is_key(Receiver, Clients),
    if IsClientSender == true andalso IsClientReceiver ->
        Client = maps:get(Receiver, Clients),
        ReceiverStatus = maps:get(status, Client),
        if ReceiverStatus == offline ->
            PersonalHistory = maps:get(personalHistory, Client),
            CurrentMsg = {"[" ++ get_time() ++ "]", Sender, Message},
            TempPersonalHistory = lists:append(PersonalHistory, [CurrentMsg]),
            TempClient = maps:put(personalHistory, TempPersonalHistory, Client),
            TempMap = maps:put(Receiver, TempClient, Clients),
            global:send(Sender, {Sender, Receiver, offline_message}),
            loop(ServerName, TempMap, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
        true ->
            global:send(Receiver, {Sender, Receiver, Message, private_message}),
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
        end;
    true ->
        global:send(Sender, {Receiver, "Client Name not found. Cannot Send Message to Client ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec connected_clients(atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

connected_clients(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    ClientsList = maps:keys(Clients),
    io:format("\n[" ++ get_time() ++ "] Clients Connected to Server :~p~n", [ClientsList]),
    loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic).

-spec connected_clients_client(atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

connected_clients_client(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    ClientsList = maps:keys(Clients),
    global:send(ClientName, {ServerName, ClientsList, connected_clients}),
    loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic).

-spec msg_history(atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

msg_history(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    DisplayHistory = lists:reverse(MsgHistory),
    io:format("\n[" ++ get_time() ++ "]"),
    io:format("Chat Message History :~n"),
    lists:foreach(fun({Time, From, Message}) ->
    io:format("~p ~p: ~p~n",[Time, From, Message]) end, DisplayHistory),
    loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic).

-spec msg_history_client(atom(), atom(),map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> ok | no_return().

msg_history_client(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsClient = maps:is_key(ClientName, Clients),
    if IsClient == true ->
        global:send(ClientName, {ServerName, MsgHistory, full_history}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
    true -> 
        global:send(ClientName, {ServerName, "Client Name not found. Cannot get History of the Messages on the Server ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec update_admin_only_topic(atom(), boolean(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> ok | no_return().

update_admin_only_topic(ClientName, EnableAdminOnly, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsClient = maps:is_key(ClientName, Clients),
    if IsClient == true ->
        Client = maps:get(ClientName, Clients),
        IsAdmin = maps:get(isAdmin, Client),
        if IsAdmin == true ->
             NewAdminOnlyTopic = EnableAdminOnly,
             io:format("\n[" ++ get_time() ++ "] "),
             if EnableAdminOnly == true ->
                io:format("Admin-only topic change enabled by ~p~n", [ClientName]);
            true ->
                io:format("Admin-only topic change disabled by ~p~n", [ClientName])
            end,
            Message = "Admin-only topic change is now " ++ (if EnableAdminOnly == true -> "enabled"; true -> "disabled" end) ++ " by " ++ atom_to_list(ClientName),
            global:send(ServerName, {Message, send_message_to_all_clients}),
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, NewAdminOnlyTopic);
            true ->
                global:send(ClientName, {ServerName, "You are Not Admin. Only admins can change the admin-only topic setting of the .", connection_failed}),
                loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
            end;
        true -> 
            global:send(ClientName, {ServerName, "Client Name not found. Cannot Update the Admin-only topic change switch on the Server ", connection_failed}),
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec update_topic_client(atom(), string(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> ok | no_return().

update_topic_client(ClientName, NewTopic, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsClient = maps:is_key(ClientName, Clients), 
    if IsClient == true ->
        Client = maps:get(ClientName, Clients),
        if AdminOnlyTopic == true ->
            IsAdmin = maps:get(isAdmin, Client),
            if IsAdmin == true ->
                io:format("\n[" ++ get_time() ++ "] "),
                io:format("~p Updated the Chat Room Topic changed to ~p~n",[ClientName, NewTopic]),
                Message = "Chat Room Topic changed to " ++ NewTopic,
                global:send(ServerName,{Message, send_message_to_all_clients}),
                loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, NewTopic, AdminOnlyTopic);
            true ->
                global:send(ClientName, {ServerName, "You are Not Admin. Only Admin Can Change the Topic. Failed to change the Chat Topic on the Server ", connection_failed}),
                loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
            end;
        true ->
            io:format("\n[" ++ get_time() ++ "] "),
            io:format("~p Updated the Chat Room Topic to ~p~n",[ClientName, NewTopic]),
            Message = "Chat Room Topic changed to " ++ NewTopic,
            global:send(ServerName,{Message, send_message_to_all_clients}),
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, NewTopic, AdminOnlyTopic)
        end;
    true -> 
        global:send(ClientName, {ServerName, "Client Name not found. Cannot Update the Chat Topic on the Server ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec get_topic_client(atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

get_topic_client(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    global:send(ClientName, {ServerName, Topic, current_topic}),
    loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic).

-spec send_msg_server_to_all(string(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

send_msg_server_to_all(Message, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    ClientsList = maps:keys(Clients),
    lists:foreach(fun(ClientName) -> global:send(ClientName, {ServerName, Message, announcement}) end, ClientsList),
    loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic).

-spec send_msg_server_to_all_except_given(string(), atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

send_msg_server_to_all_except_given(Message, ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    ClientsList = maps:keys(Clients),
    FilteredClients = lists:filter(fun(Name) -> Name =/= ClientName end, ClientsList),
    lists:foreach(fun(Name) -> global:send(Name, {ServerName, Message, announcement}) end, FilteredClients),
    loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic).

-spec make_admin_server(atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

make_admin_server(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsClient = maps:is_key(ClientName, Clients),
    if IsClient == true ->
        Client = maps:get(ClientName, Clients),
        TempClient = maps:put(isAdmin, true, Client),
        TempMap = maps:put(ClientName, TempClient, Clients),
        io:format("\n[" ++ get_time() ++ "] ~p promoted to Admin~n", [ClientName]),
        global:send(ClientName, {ServerName, "You are now an Admin", announcement}),
        loop(ServerName, TempMap, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
    true ->
        global:send(ServerName, {ServerName, "Client not Found. Failed to promote to Admin on the Server ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec get_admins_client(atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

get_admins_client(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    Admins = lists:filter(fun(Name) -> 
        Client = maps:get(Name, Clients),
        maps:get(isAdmin, Client)
    end, maps:keys(Clients)),
    global:send(ClientName, {ServerName, Admins, admin_list}),
    loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic).

-spec update_client_status(atom(), atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

update_client_status(ClientName, Status, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsClient = maps:is_key(ClientName, Clients),
    if IsClient == true ->
        Client = maps:get(ClientName, Clients),
        TempClient1 = maps:put(status, Status, Client),
        TempMap = maps:put(ClientName, TempClient1, Clients),
        io:format("\n[" ++ get_time() ++ "] ~p is now ~p~n", [ClientName, Status]),
        if Status =:= online ->
            PersonalHistory = maps:get(personalHistory, TempClient1),
            TempClient2 = maps:put(personalHistory, [], TempClient1),
            TempMap2 = maps:put(ClientName, TempClient2, TempMap),
            global:send(ClientName, {ServerName, PersonalHistory, offline_message_history}),
            loop(ServerName, TempMap2, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
        true -> ok
        end,
        loop(ServerName, TempMap, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
    true -> 
        global:send(ClientName, {ServerName, "Cannot Update the Client Status. Client Name not found on the Server .", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec kick(atom(), atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

kick(AdminName, ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsAdminClient = maps:is_key(AdminName, Clients),
    IsClient = maps:is_key(ClientName, Clients), 
    if IsAdminClient == true andalso IsClient == true ->
        Admin = maps:get(AdminName, Clients),
        IsAdmin = maps:get(isAdmin, Admin),
        if IsAdmin == true ->
            TempMap = maps:remove(ClientName, Clients),
            io:format("\n[" ++ get_time() ++ "] ~p kicked ~p from the chat~n", [AdminName, ClientName]),
            global:send(ClientName, {ServerName, "You have been kicked from the server", announcement}),
            loop(ServerName, TempMap, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
        true ->
            global:send(AdminName, {ServerName, "You are not an Admin. Failed to kick client from the ", connection_failed}),
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
        end;
    true ->
        global:send(AdminName, {ServerName, "Client not found. Failed to kick client from the ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec mute(atom(), atom(), integer(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

mute(AdminName, ClientName, Time, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsAdminClient = maps:is_key(AdminName, Clients),
    IsClient = maps:is_key(ClientName, Clients),
    if IsAdminClient == true andalso IsClient == true ->
        Admin = maps:get(AdminName, Clients),
        IsAdmin = maps:get(isAdmin, Admin),
        if IsAdmin == true ->
            {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
            MuteEndTime = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}) + Time * 60,
            MuteEndDatetime = calendar:gregorian_seconds_to_datetime(MuteEndTime),
            {{MuteYear, MuteMonth, MuteDay}, {MuteHour, MuteMin, MuteSec}} = MuteEndDatetime,
            MuteTimeStr = lists:concat([MuteYear, "/", MuteMonth, "/", MuteDay, " ", MuteHour, ":", MuteMin, ":", MuteSec]),
            Client = maps:get(ClientName, Clients),
            MutedClient = maps:put(isMuted, true, maps:put(mutedTill, MuteEndDatetime, Client)),
            TempMap = maps:put(ClientName, MutedClient, Clients),
            io:format("\n[" ++ get_time() ++ "] ~p Muted ~p in the Group Till ~p~n", [AdminName, ClientName, MuteTimeStr]),
            global:send(ClientName, {ServerName, "You have been muted until " ++ MuteTimeStr, announcement}),
            timer:send_after(Time*60*1000, self(), {AdminName, ClientName, unmute_client}),
            loop(ServerName, TempMap, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
        true ->
            global:send(AdminName, {ServerName, "You are not an Admin. Failed to mute client on the ", connection_failed}),
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
        end;
    true ->
        global:send(AdminName, {ServerName, "Client not found. . Failed to mute client on the ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec unmute(atom(), atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

unmute(AdminName, ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsAdminClient = maps:is_key(AdminName, Clients),
    IsClient = maps:is_key(ClientName, Clients),
    if IsAdminClient == true andalso IsClient == true ->
        Admin = maps:get(AdminName, Clients),
        IsAdmin = maps:get(isAdmin, Admin),
        if IsAdmin == true ->
            TempClient = maps:put(isMuted, false, maps:get(ClientName, Clients)),
            TempMap = maps:put(ClientName, TempClient, Clients),
            io:format("\n[" ++ get_time() ++ "] ~p unmuted ~p~n", [AdminName, ClientName]),
            global:send(ClientName, {ServerName, "You have been unmuted", announcement}),
            loop(ServerName, TempMap, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
        true ->
            global:send(AdminName, {ServerName, "You are not an Admin. Failed to unmute client on the ", connection_failed}),
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
        end;
    true ->
        global:send(AdminName, {ServerName, "Client not found. Failed to unmute client on the ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec make_admin_client(atom(), atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

make_admin_client(AdminName, ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsAdminClient = maps:is_key(AdminName, Clients),
    IsClient = maps:is_key(ClientName, Clients),
    if IsAdminClient == true andalso IsClient == true ->
        Admin = maps:get(AdminName, Clients),
        IsAdmin = maps:get(isAdmin, Admin),
        if IsAdmin == true ->
            TempClient = maps:put(isAdmin, true, maps:get(ClientName, Clients)),
            TempMap = maps:put(ClientName, TempClient, Clients),
            io:format("\n[" ++ get_time() ++ "] ~p promoted ~p to Admin~n", [AdminName, ClientName]),
            global:send(ClientName, {ServerName, "You have been promoted to Admin", announcement}),
            loop(ServerName, TempMap, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
        true ->
            global:send(AdminName, {ServerName, "You are not an Admin. Failed to promote to Admin on the ", connection_failed}),
            loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
        end;
    true ->
        global:send(AdminName, {ServerName, "Client not found. Failed to promote to Admin on the ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec disconnect(atom(), atom(), map(), pos_integer(), pos_integer(), list(), string(), boolean()) -> no_return().

disconnect(ClientName, ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic) ->
    IsClient = maps:is_key(ClientName, Clients),
    if IsClient == true ->
        TempMap = maps:remove(ClientName, Clients),
        io:format("\n[" ++ get_time() ++ "] ~p disconnected from the server~n", [ClientName]),
        Message = atom_to_list(ClientName) ++ " has left the chat",
        global:send(ServerName, {Message, ClientName, send_message_to_all_clients_except_given}),
        loop(ServerName, TempMap, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic);
    true ->
        global:send(ClientName, {ServerName, "Client not found. Failed to disconnect from the ", connection_failed}),
        loop(ServerName, Clients, MaxClients, MaxHistoryCount, MsgHistory, Topic, AdminOnlyTopic)
    end.

-spec get_time() -> string().

get_time() ->
  {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
  Time = lists:concat([Year,'/',Month,'/',Day,' ',Hour,':',Min,':',Sec]),
  Time.

-spec get_history(atom()) -> ok.

get_history(ServerName) ->
  global:send(ServerName,{history}).

-spec get_connected_clients(atom()) -> ok.

get_connected_clients(ServerName) ->
  global:send(ServerName, {get_connected_clients}).

-spec send_message_to_all_clients(atom(), string()) -> ok.

send_message_to_all_clients(ServerName, Msg) ->
  global:send(ServerName, {Msg,send_message_to_all_clients}).

-spec make_admin(atom(), atom()) -> ok.

make_admin(ServerName, ClientName) ->
  global:send(ServerName, {ClientName, make_admin_server}).