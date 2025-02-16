-module(chat_client).
-export([start/2, send_message/3, disconnect/2, send_private_message/4, get_connected_clients/2, update_chat_topic/3, get_current_chat_topic/2, kick/3, mute/4, unmute/3, make_admin/3, get_admins/2, set_status/3, get_full_history/2, update_admin_only_topic_change/3]).

-spec start(atom(), atom()) -> ok.

start(ServerName, ClientName) ->
    ServerPid = global:whereis_name(ServerName),
    case ServerPid of
        undefined ->
            io:format("Error: Server ~p not found!~n", [ServerName]);
        _ ->
            ClientPid = spawn(fun() -> receives(ClientName) end),
            io:format("ClientPid ~p ServerPid ~p~n", [ClientPid, ServerPid]),
            global:register_name(ClientName, ClientPid),
            ServerPid ! {ClientName, ClientPid, connect_client}
    end.

-spec receives(atom()) -> no_return().

receives(ClientName) ->
    receive
        {Name, Msg, connection_failed} ->
            io:format("~p~p~n", [Msg, Name]),
            receives(ClientName);
        {Sender, _ReceiverName, Msg, private_message} ->
            io:format("[Private Message][~p]: ~p~n", [Sender, Msg]),
            receives(ClientName);
        {ServerName, Clients, connected_clients} ->
            io:format("All Connected Clients to Server ~p : ~p~n", [ServerName, Clients]),
            receives(ClientName);
        {ServerName, MsgHistory, MaxHistoryCount, latest_n_history} ->
            io:format("Welcome to Chat Room~n"),
            io:format("Chat Message History of Last ~p Messages from Server ~p :~n", [MaxHistoryCount, ServerName]),
            DisplayList = lists:reverse(MsgHistory),
            lists:foreach(fun({Time, From, Message}) -> io:format("~p ~p: ~p~n", [Time, From, Message]) end, lists:sublist(DisplayList, MaxHistoryCount)),
            receives(ClientName);
        {ServerName, MsgHistory, full_history} ->
            io:format("Chat Message History from Server ~p:~n", [ServerName]),
            lists:foreach(fun({Time, From, Message}) -> io:format("~p ~p: ~p~n", [Time, From, Message]) end, lists:reverse(MsgHistory)),
            receives(ClientName);
        {ServerName, Topic, current_topic} ->
            io:format("Current Chat Room Topic : ~p on Server : ~p~n", [Topic, ServerName]),
            receives(ClientName);
        {ServerName, Msg, announcement} ->
            io:format("[Server ~p] ~p~n", [ServerName, Msg]),
            receives(ClientName);
        {ServerName, AdminKeys, admin_list} ->
            io:format("All Admin Clients on Server ~p : ~p~n", [ServerName, AdminKeys]),
            receives(ClientName);
        {_Sender, ReceiverName, offline_message} ->
            io:format("~p is offline and will receive messages once back.~n", [ReceiverName]),
            receives(ClientName);
        {_ServerName, MsgHistory, offline_message_history} ->
            io:format("Private Chat Message History you Received When you were offline:~n"),
            lists:foreach(fun({Time, From, Message}) -> io:format("~p ~p: ~p~n", [Time, From, Message]) end, lists:reverse(MsgHistory)),
            receives(ClientName);
        _ ->
            io:format("Unknown message received.~n"),
            receives(ClientName)
    end.

-spec send_message(atom(), atom(), string()) -> ok.

send_message(ServerName, ClientName, Msg) ->
    global:send(ServerName, {ClientName, Msg, send_message}).

-spec disconnect(atom(), atom()) -> ok.

disconnect(ServerName, ClientName) ->
    global:send(ServerName, {ClientName, disconnect_client}).

-spec send_private_message(atom(), atom(), atom(), string()) -> ok.

send_private_message(ServerName, ClientName, ReceiverName, Msg) ->
    global:send(ServerName, {ClientName, ReceiverName, Msg, send_private_message}).

-spec get_connected_clients(atom(), atom()) -> ok.

get_connected_clients(ServerName, ClientName) ->
    global:send(ServerName, {ClientName, get_connected_clients_client}).

-spec update_chat_topic(atom(), atom(), string()) -> ok.

update_chat_topic(ServerName, ClientName, NewTopic) ->
    global:send(ServerName, {ClientName, NewTopic, update_topic}).

-spec get_current_chat_topic(atom(), atom()) -> ok.

get_current_chat_topic(ServerName, ClientName) ->
    global:send(ServerName, {ClientName, get_current_topic}).

-spec kick(atom(), atom(), atom()) -> ok.

kick(ServerName, AdminName, ClientName) ->
    global:send(ServerName, {AdminName, ClientName, kick_client}).

-spec mute(atom(), atom(), atom(), pos_integer()) -> ok.

mute(ServerName, AdminName, ClientName, Time) ->
    global:send(ServerName, {AdminName, ClientName, Time, mute_client}).

-spec unmute(atom(), atom(), atom()) -> ok.

unmute(ServerName, AdminName, ClientName) ->
    global:send(ServerName, {AdminName, ClientName, unmute_client}).

-spec make_admin(atom(), atom(), atom()) -> ok.

make_admin(ServerName, AdminName, ClientName) ->
    global:send(ServerName, {AdminName, ClientName, make_admin_client}).

-spec get_admins(atom(), atom()) -> ok.

get_admins(ServerName, ClientName) ->
    global:send(ServerName, {ClientName, get_admins}).

-spec set_status(atom(), atom(), string()) -> ok.

set_status(ServerName, ClientName, Status) ->
    global:send(ServerName, {ClientName, Status, update_status}).

-spec get_full_history(atom(), atom()) -> ok.

get_full_history(ServerName, ClientName) ->
    global:send(ServerName, {ClientName, history_client}).

-spec update_admin_only_topic_change(atom(), atom(), boolean()) -> ok.

update_admin_only_topic_change(ServerName, ClientName, EnableAdminOnly) ->
    global:send(ServerName, {ClientName, EnableAdminOnly, updt_admin_only_topic}).
