-module(cs).
-behaviour(gen_server).

-export([start/2, stop/0, connect/2, send_message/2, list_clients/0, private_message/3, 
         set_topic/2, get_topic/0, kick/2, mute/3, unmute/2, get_admins/0, promote_admin/2]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).

-record(state, {
    clients = #{},            % {Username => Pid}
    messages = [],            % [{Sender, Message, Timestamp}]
    max_clients,              % Maximum allowed clients
    topic = "Welcome!",       % Chat topic
    admins = #{"admin" => true}, % Admin users
    muted_users = #{},        % {Username => UnmuteTime}
    offline_messages = #{},   % {Receiver => [{Sender, Message, Timestamp}]}
    max_messages              % Maximum number of messages to send to new clients
}).

start(MaxClients, MaxMessages) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [MaxClients, MaxMessages], []).

stop() ->
    gen_server:call(?MODULE, stop).

connect(ClientName, Pid) ->
    gen_server:call(?MODULE, {connect, ClientName, Pid}).

send_message(ClientName, Message) ->
    gen_server:call(?MODULE, {message, ClientName, Message}).

list_clients() ->
    gen_server:call(?MODULE, list_clients).

private_message(Sender, Receiver, Message) ->
    gen_server:call(?MODULE, {private_message, Sender, Receiver, Message}).

set_topic(ClientName, Topic) ->
    gen_server:call(?MODULE, {set_topic, ClientName, Topic}).

get_topic() ->
    gen_server:call(?MODULE, get_topic).

kick(AdminName, ClientName) ->
    gen_server:call(?MODULE, {kick, AdminName, ClientName}).

mute(AdminName, ClientName, Time) ->
    gen_server:call(?MODULE, {mute, AdminName, ClientName, Time}).

unmute(AdminName, ClientName) ->
    gen_server:call(?MODULE, {unmute, AdminName, ClientName}).

get_admins() ->
    gen_server:call(?MODULE, get_admins).

promote_admin(AdminName, ClientName) ->
    gen_server:call(?MODULE, {promote_admin, AdminName, ClientName}).

init([MaxClients, MaxMessages]) ->
    {ok, #state{max_clients = MaxClients, max_messages = MaxMessages}}.

handle_call(list_clients, _From, State) ->
    {reply, maps:keys(State#state.clients), State};

handle_call({message, ClientName, Message}, _From, State) ->
    CurrentTime = erlang:system_time(),
    case maps:find(ClientName, State#state.muted_users) of
        {ok, UnmuteTime} when CurrentTime < UnmuteTime ->
            {reply, {error, "You are muted"}, State};
        _ ->
            case maps:find(ClientName, State#state.clients) of
                {ok, _Pid} ->
                    Timestamp = CurrentTime,
                    UpdatedMessages = [{ClientName, Message, Timestamp} | State#state.messages],
                    TrimmedMessages = lists:sublist(UpdatedMessages, min(length(UpdatedMessages), State#state.max_messages)),
                    broadcast(State, io_lib:format("~s: ~s", [ClientName, Message])),
                    {reply, ok, State#state{messages = TrimmedMessages}};
                error ->
                    {reply, {error, "Client not found"}, State}
            end
    end;

handle_call({private_message, Sender, Receiver, Message}, _From, State) ->
    case maps:find(Receiver, State#state.clients) of
        {ok, Pid} ->
            Pid ! {private, Sender, Message},
            {reply, ok, State};
        error ->
            NewOfflineMessages = maps:update_with(Receiver, fun(Msgs) -> [{Sender, Message, erlang:system_time()} | Msgs] end, [{Sender, Message, erlang:system_time()}], State#state.offline_messages),
            SenderPid = maps:get(Sender, State#state.clients, undefined),
            case SenderPid of
                undefined -> ok;
                _ -> SenderPid ! {offline_message, Receiver, "Message will be delivered when they connect."}
            end,
            {reply, {offline, Receiver}, State#state{offline_messages = NewOfflineMessages}}
    end;

handle_call({connect, ClientName, Pid}, _From, State) ->
    case maps:is_key(ClientName, State#state.clients) of
        true -> {reply, {error, "Username already taken"}, State};
        false when map_size(State#state.clients) >= State#state.max_clients ->
            {reply, {error, "Server is full"}, State};
        false ->
            UpdatedClients = maps:put(ClientName, Pid, State#state.clients),
            NewState = State#state{clients = UpdatedClients},
            broadcast(State, io_lib:format("~s has joined the chat", [ClientName])),
            MessagesToSend = lists:sublist(State#state.messages, min(length(State#state.messages), State#state.max_messages)),
            NewStateAfterAdminCheck = 
                case map_size(State#state.clients) of
                    0 -> NewState#state{admins = maps:put(ClientName, true, NewState#state.admins)};
                    _ -> NewState
                end,
            case maps:find(ClientName, State#state.offline_messages) of
                {ok, Messages} ->
                    lists:foreach(fun({Sender, Msg, _}) -> Pid ! {private, Sender, Msg} end, Messages),
                    NewOfflineMessages = maps:remove(ClientName, State#state.offline_messages),
                    {reply, {ok, MessagesToSend, State#state.topic}, NewStateAfterAdminCheck#state{offline_messages = NewOfflineMessages}};
                error ->
                    {reply, {ok, MessagesToSend, State#state.topic}, NewStateAfterAdminCheck}
            end
    end;

handle_call(get_topic, _From, State) ->
    {reply, {ok, State#state.topic}, State};

handle_call({set_topic, ClientName, Topic}, _From, State) ->
    case maps:get(ClientName, State#state.admins, false) of
        true ->
            broadcast(State, io_lib:format("Topic changed to: ~s", [Topic])),
            {reply, ok, State#state{topic = Topic}};
        false ->
            {reply, {error, "Only admins can change topic"}, State}
    end;

handle_call({promote_admin, AdminName, ClientName}, _From, State) ->
    case maps:get(AdminName, State#state.admins, false) of
        true ->
            case maps:get(ClientName, State#state.admins, false) of
                true -> {reply, {error, "User is already an admin"}, State};
                false ->
                    case maps:get(ClientName, State#state.clients, undefined) of
                        undefined -> {reply, {error, "User not found"}, State};
                        _ -> {reply, ok, State#state{admins = maps:put(ClientName, true, State#state.admins)}}
                    end
            end;
        false -> {reply, {error, "Only admins can promote users as admins"}, State}
    end;

handle_call({kick, AdminName, ClientName}, _From, State) ->
    case maps:get(AdminName, State#state.admins, false) of
        true ->
            case maps:find(ClientName, State#state.clients) of
                {ok, _} ->
                    NewClients = maps:remove(ClientName, State#state.clients),
                    broadcast(State, io_lib:format("~s has been kicked by ~s", [ClientName, AdminName])),
                    {reply, ok, State#state{clients = NewClients}};
                error -> {reply, {error, "User not found"}, State}
            end;
        false -> {reply, {error, "Only admins can kick users"}, State}
    end;

handle_call({mute, AdminName, ClientName, Time}, _From, State) ->
    case maps:get(AdminName, State#state.admins, false) of
        true ->
            UnmuteTime = erlang:system_time() + Time,
            NewMutedUsers = maps:put(ClientName, UnmuteTime, State#state.muted_users),
            {reply, ok, State#state{muted_users = NewMutedUsers}};
        false -> {reply, {error, "Only admins can mute users"}, State}
    end;

handle_call({unmute, AdminName, ClientName}, _From, State) ->
    case maps:get(AdminName, State#state.admins, false) of
        true ->
            NewMutedUsers = maps:remove(ClientName, State#state.muted_users),
            {reply, ok, State#state{muted_users = NewMutedUsers}};
        false -> {reply, {error, "Only admins can unmute users"}, State}
    end;

handle_call(get_admins, _From, State) ->
    {reply, maps:keys(State#state.admins), State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

broadcast(State, Msg) ->
    maps:foreach(fun(_, Pid) -> Pid ! {broadcast, Msg} end, State#state.clients).
