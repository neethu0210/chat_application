-module(cc).
-export([start/1, send_message/2, private_message/3, get_clients/1]).

start(Name) ->
    case cs:connect(Name, self()) of
        {ok, _Messages, _Topic} ->
            io:format("Connected to the chat~n"),
            Pid = spawn(fun() -> init(Name) end),
            Pid;

        {error, Reason} ->
            io:format("Failed to connect: ~s~n", [Reason]),
            undefined % Return 'undefined' to indicate failure
    end.

send_message(Pid, Msg) ->
    Pid ! {send, Msg}.

private_message(Pid, Receiver, Msg) ->
    Pid ! {private, Receiver, Msg}.

get_clients(Pid) ->
    Pid ! list_clients.

init(Name) ->
    receive
        {ok, Messages, Topic} ->
            io:format("Connected to chat. Topic: ~s~n", [Topic]),
            lists:foreach(fun({User, Msg, _}) -> io:format("~s: ~s~n", [User, Msg]) end, Messages),
            loop(Name);

        {error, Reason} ->
            io:format("Failed to connect: ~s~n", [Reason]),
            exit(normal);

        _ -> init(Name)
    end.

loop(Name) ->
    receive
        {broadcast, Msg} -> io:format("~s~n", [Msg]);
        {private, From, Msg} -> io:format("Private from ~s: ~s~n", [From, Msg]);
        {muted, Until} -> io:format("You are muted until ~p~n", [Until]);
        {kicked, Reason} -> io:format("You were kicked: ~s~n", [Reason]), exit(normal);
        {server_down} -> io:format("Server is shutting down~n"), exit(normal);
        list_clients -> cs:list_clients();
        {send, Msg} -> cs:send_message(Name, Msg);
        {private, Receiver, Msg} -> cs:private_message(Name, Receiver, Msg)
    end,
    loop(Name).
