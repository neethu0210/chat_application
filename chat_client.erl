-module(chat_client).
-export([start/1, connect/1, send_message/2, send_private_message/3, get_history/0, loop/1, set_topic/2, get_topic/0, kick/2, mute/3, unmute/2, get_admins/0, promote_admin/2, disconnect/1, list_clients/0, set_topic_restriction/1]).

start(Name) ->
    spawn(?MODULE, connect, [Name]).

connect(Name) -> 
    case cs:connect(Name, self()) of
        {ok, Messages, Topic} -> 
            if Topic == "" ->  io:format("[~s] Connected to the chat. No topic is set.~n", [Name]);
            true -> io:format("[~s] Connected to the chat. Topic: ~s~n", [Name, Topic])
            end,
            lists:foreach(fun({Sender, Msg, Timestamp}) -> 
                HumanReadableTime = calendar:system_time_to_rfc3339(Timestamp div 1000000, [{unit, millisecond}]),
                io:format("[~p] ~p: ~p~n", [HumanReadableTime, Sender, Msg]) 
            end, Messages),

            receive_offline_messages(Name),
            loop(Name);
        
        {error, Reason} -> 
            io:format("Failed to Connect: ~s~n", [Reason])
    end.

receive_offline_messages(Name) ->
    receive
        {private, Sender, Message} ->
            io:format("[~s] ~s: ~s~n", [Name, Sender, Message]),
            receive_offline_messages(Name)
    after 500 ->
        ok
    end.

disconnect(ClientName) ->
    case cs:disconnect(ClientName) of
        ok -> io:format("[~s] You have left the chat.~n", [ClientName]);
        {error, Reason} -> io:format("Failed to disconnect: ~s~n", [Reason])
    end.

send_message(Name, Message) ->
    case cs:send_message(Name, Message) of
        ok -> io:format("Message sent successfully.~n");
        {error, Reason} -> io:format("Failed to send message: ~s~n", [Reason])
    end.

send_private_message(Sender, Receiver, Message) ->
    case cs:private_message(Sender, Receiver, Message) of
        ok -> io:format("Private message sent to ~s.~n", [Receiver]);
        {offline, _} -> io:format("User ~s is offline.~n", [Receiver]);
        {error, Reason} -> io:format("Failed to send private message: ~s~n", [Reason])
    end.

get_history() ->
    Messages = cs:get_history(),
    lists:foreach(fun({Sender, Msg, Timestamp}) -> 
        io:format("[~p] ~p: ~p~n", [Timestamp, Sender, Msg]) 
    end, Messages).

get_admins() ->
    Admins = cs:get_admins(),
    io:format("Admins: ~p~n", [Admins]).

list_clients() ->
    Clients = cs:list_clients(),
    io:format("Connected Clients: ~p~n", [Clients]).

get_topic() ->
    case cs:get_topic() of
        {ok, Topic} -> 
            if Topic == "" -> io:format("No topic is set~n");
            true -> io:format("Current topic: ~s~n", [Topic])
            end;
        _ -> io:format("Unable to fetch topic~n.")
    end.

set_topic(User, NewTopic) ->
    case cs:set_topic(User, NewTopic) of
        ok -> io:format("[~s] Topic changed to: ~s~n", [User, NewTopic]);
        {error, Reason} -> io:format("Failed to set topic: ~s~n", [Reason])
    end.

set_topic_restriction(Admin) ->
    io:format("Enable admin-only topic mode? (yes/no): "),
    case io:get_line("") of
        "yes\n" -> Restriction = true;
        "no\n" -> Restriction = false;
        _ -> io:format("Invalid input.~n"), Restriction = undefined
    end,
    if Restriction =/= undefined ->
        case cs:set_topic_restriction(Admin, Restriction) of
            ok -> io:format("Topic restriction updated successfully.~n");
            {error, Reason} -> io:format("Failed to update topic restriction: ~s~n", [Reason])
        end;
    true -> ok
    end.

kick(Admin, User) ->
    case cs:kick(Admin, User) of
        ok -> 
            io:format("[~s] User ~s has been kicked from the chat.~n", [Admin, User]),
            io:format("[~s] You have been kicked from the chat by ~s.~n", [User, Admin]);
        {error, Reason} -> io:format("Failed to kick user: ~s~n", [Reason])
    end.

mute(Admin, User, Time) ->
    case cs:mute(Admin, User, Time) of
        ok -> io:format("User ~s has been muted.~n", [User]);
        {error, Reason} -> io:format("Failed to mute user: ~s~n", [Reason])
    end.

unmute(Admin, User) ->
    case cs:unmute(Admin, User) of
        ok -> io:format("User ~s has been unmuted.~n", [User]);
        {error, Reason} -> io:format("Failed to unmute user: ~s~n", [Reason])
    end.

promote_admin(Admin, User) ->
    case cs:promote_admin(Admin, User) of
        ok -> io:format("User ~s is now an admin.~n", [User]);
        {error, Reason} -> io:format("Failed to promote user: ~s~n", [Reason])
    end.

loop(Name) ->
    receive
        {broadcast, Msg} ->
            io:format("~s~n", [Msg]),
            loop(Name);
        
        _ ->
            loop(Name)
    end.