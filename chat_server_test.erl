-module(chat_server_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    ServerName = test_chat_server,
    MaxClients = 3,
    MaxHistoryCount = 5,
    chat_server:start(ServerName, MaxClients, MaxHistoryCount),
    {ok, ServerName}.

teardown(ServerName) ->
    Pid = global:whereis_name(ServerName),
    if
        Pid =/= undefined -> exit(Pid, kill);
        true -> ok
    end,
    ok.

start_test(_) ->
    ServerName = test_start_server,
    MaxClients = 3,
    MaxHistoryCount = 5,
    chat_server:start(ServerName, MaxClients, MaxHistoryCount),
    Pid = global:whereis_name(ServerName),
    ?assert(Pid =/= undefined).

get_history_test(ServerName) ->
    ClientName1 = test_client1,
    ClientPid1 = spawn(fun() -> receive after infinity -> ok end end),
    global:register_name(ClientName1, ClientPid1),
    global:send(ServerName, {ClientName1, ClientPid1, connect_client}),

    ClientName2 = test_client2,
    ClientPid2 = spawn(fun() -> receive after infinity -> ok end end),
    global:register_name(ClientName2, ClientPid2),
    global:send(ServerName, {ClientName2, ClientPid2, connect_client}),

    chat_client:send_message(ServerName, ClientName1, "Hello World"),
    chat_client:send_message(ServerName, ClientName2, "Hello All"),

    Details = chat_server:get_details(ServerName),
    MsgHistory = maps:get(message_history, Details),
    ExpectedHistory = [{ClientName1, "Hello World"}, {ClientName2, "Hello All"}],
    ExtractedHistory = lists:map(fun({_, C, M}) -> {C, M} end, MsgHistory),
    ?assertEqual(lists:reverse(ExpectedHistory), ExtractedHistory).

get_connected_clients_test(ServerName) ->
    ClientName1 = test_client1,
    ClientPid1 = spawn(fun() -> receive after infinity -> ok end end),
    global:register_name(ClientName1, ClientPid1),
    global:send(ServerName, {ClientName1, ClientPid1, connect_client}),

    ClientName2 = test_client2,
    ClientPid2 = spawn(fun() -> receive after infinity -> ok end end),
    global:register_name(ClientName2, ClientPid2),
    global:send(ServerName, {ClientName2, ClientPid2, connect_client}),

    Details = chat_server:get_details(ServerName),
    Clients = maps:get(clients, Details),
    ?assert(maps:is_key(ClientName1, Clients) andalso maps:is_key(ClientName2, Clients)).

make_admin_test(ServerName) ->
    ClientName = test_client,
    ClientPid = spawn(fun() -> receive after infinity -> ok end end),
    global:register_name(ClientName, ClientPid),
    global:send(ServerName, {ClientName, ClientPid, connect_client}),

    chat_server:make_admin(ServerName, ClientName),
    Details = chat_server:get_details(ServerName),
    Clients = maps:get(clients, Details),
    Admin = maps:get(is_admin, maps:get(ClientName, Clients)),
    ?assertEqual(true, Admin).

instantiator({ok, ServerName}) ->
    [
        fun() -> start_test(ServerName) end,
        fun() -> get_history_test(ServerName) end,
        fun() -> get_connected_clients_test(ServerName) end,
        fun() -> make_admin_test(ServerName) end
    ].

all_test_() ->
    {setup, fun setup/0, fun teardown/1, fun instantiator/1}.
