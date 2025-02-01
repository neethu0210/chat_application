-module(cs).
-behaviour(gen_server).

-export([start_link/1, stop/0, connect/2, disconnect/1]).
-export([send_message/2, send_private_message/3, get_clients/0, get_history/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    clients = #{},
    history = [],
    max_clients
}).

start_link(MaxClients) ->
    gen_server:start_link({local, cs}, cs, [MaxClients], []).

stop() ->
    gen_server:call(cs, stop).

connect(Username, Pid) ->
    gen_server:call(cs, {connect, Username, Pid}).

disconnect(Username) ->
    gen_server:cast(cs, {disconnect, Username}).

send_message(Username, Message) ->
    gen_server:cast(cs, {message, Username, Message}).

send_private_message(FromUser, ToUser, Message) ->
    gen_server:cast(cs, {private_message, FromUser, ToUser, Message}).

get_clients() ->
    gen_server:call(cs, get_clients).

get_history() ->
    gen_server:call(cs, get_history).

init([MaxClients]) ->
    {ok, #state{max_clients = MaxClients}}.

handle_call({connect, Username, Pid}, _From, State) ->
    case maps:is_key(Username, State#state.clients) of
        true -> {reply, {error, "Username already taken"}, State};
        false -> 
            case maps:size(State#state.clients) >= State#state.max_clients of
                true -> {reply, {error, "Server is full"}, State};
                false ->
                    NewClients = maps:put(Username, Pid, State#state.clients),
                    Timestamp = get_timestamp(),
                    EntryMessage = format_message("SERVER", Username ++ " joined the chat", Timestamp),
                    NewHistory = [EntryMessage | State#state.history],
                    broadcast(NewClients, EntryMessage),
                    Pid ! {chat_history, NewHistory},
                    {reply, {ok, "Connected"}, State#state{clients = NewClients, history = NewHistory}}
            end
    end;

handle_call(get_clients, _From, State) ->
    {reply, maps:keys(State#state.clients), State};

handle_call(get_history, _From, State) ->
    {reply, State#state.history, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({disconnect, Username}, State) ->
    case maps:take(Username, State#state.clients) of
        {Pid, NewClients} ->
            Timestamp = get_timestamp(),
            ExitMessage = format_message("SERVER", Username ++ " left the chat", Timestamp),
            NewHistory = [ExitMessage | State#state.history],
            broadcast(NewClients, ExitMessage),
            {noreply, State#state{clients = NewClients, history = NewHistory}};
        error -> {noreply, State}
    end;

handle_cast({message, Username, Message}, State) ->
    Timestamp = get_timestamp(),
    FullMessage = format_message(Username, Message, Timestamp),
    NewHistory = [FullMessage | State#state.history],
    broadcast(State#state.clients, FullMessage),
    {noreply, State#state{history = NewHistory}};

handle_cast({private_message, FromUser, ToUser, Message}, State) ->
    case maps:find(ToUser, State#state.clients) of
        {ok, ToPid} ->
            Timestamp = get_timestamp(),
            PrivateMsg = format_message(FromUser, Message, Timestamp),
            ToPid ! {private_message, PrivateMsg},
            {noreply, State};
        error ->
            FromPid = maps:get(FromUser, State#state.clients, undefined),
            case FromPid of
                undefined -> {noreply, State};
                _ -> FromPid ! {error, "User not found"}, {noreply, State}
            end
    end.

handle_info(_, State) ->
    {noreply, State}.

broadcast(Clients, Message) ->
    maps:foreach(fun(_, Pid) -> Pid ! {new_message, Message} end, Clients).

get_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    io_lib:format("[~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC]", 
                  [Year, Month, Day, Hour, Min, Sec]).

format_message(User, Message, Timestamp) ->
    lists:flatten(io_lib:format("~s ~s: ~s", [Timestamp, User, Message])).
