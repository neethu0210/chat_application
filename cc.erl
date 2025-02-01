-module(cc).
-export([start/1, stop/0, send_message/1, send_private_message/2, get_clients/0, loop/1]).

start(Username) ->
    case cs:connect(Username, self()) of
        {ok, "Connected"} ->
            Pid = spawn(fun() -> loop(Username) end),
            put(registered_client, Pid),  % Store client process in dictionary
            io:format("Connected as ~s.~n", [Username]);
        {error, Reason} ->
            io:format("Connection failed: ~s~n", [Reason]),
            exit(normal)
    end.

stop() ->
    case get(registered_client) of
        undefined -> io:format("Not connected.~n");
        Pid ->
            erase(registered_client), % Remove from dictionary
            cs:disconnect(Pid),
            Pid ! stop,
            io:format("Disconnected.~n")
    end.

send_message(Message) ->
    case get(registered_client) of
        undefined -> io:format("[ERROR] Not connected.~n");
        Pid -> Pid ! {send_message, Message}
    end.

send_private_message(ToUser, Message) ->
    case get(registered_client) of
        undefined -> 
            io:format("[ERROR] Not connected.~n");
        Pid -> 
            Clients = cs:get_clients(),
            case lists:member(ToUser, Clients) of
                true -> Pid ! {send_private_message, ToUser, Message};
                false -> io:format("[ERROR] User ~s is not connected.~n", [ToUser])
            end
    end.

get_clients() ->
    io:format("Connected users: ~p~n", [cs:get_clients()]).

loop(Username) ->
    receive
        {send_message, Message} ->
            cs:send_message(Username, Message),
            loop(Username);
        {send_private_message, ToUser, Message} ->
            cs:send_private_message(Username, ToUser, Message),
            loop(Username);
        {new_message, Message} ->
            io:format("[CHAT] ~s~n", [Message]),
            loop(Username);
        {private_message, PrivateMsg} ->
            io:format("[PRIVATE] ~s~n", [PrivateMsg]),
            loop(Username);
        {chat_history, History} ->
            io:format("[CHAT HISTORY]~n"),
            lists:foreach(fun(M) -> io:format("~s~n", [M]) end, History),
            loop(Username);
        {error, Msg} ->
            io:format("[ERROR] ~s~n", [Msg]),
            loop(Username);
        stop ->
            io:format("Client ~s disconnected.~n", [Username])
    end.
