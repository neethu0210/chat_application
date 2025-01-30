-module(cs).
-behaviour(gen_server).

-export([start_link/2, stop/0, connect/2, disconnect/1]).
-export([send_message/2, send_private_message/3, get_clients/0, get_history/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    clients = #{},
    history = [],
    max_clients,
    max_history
}).

start_link(MaxClients, MaxHistory) ->
    gen_server:start_link({local, cs}, cs, [MaxClients, MaxHistory], []).

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

init([MaxClients, MaxHistory]) ->
    {ok, #state{max_clients = MaxClients, max_history = MaxHistory}}.

handle_call({connect, Username, Pid}, _From, State) ->
    case maps:size(State#state.clients) >= State#state.max_clients of
        true -> {reply, {error, "Server is full"}, State};
        false ->
            case maps:is_key(Username, State#state.clients) of
                true -> {reply, {error, "Username already taken"}, State};
                false ->
                    NewClients = maps:put(Username, Pid, State#state.clients),
                    broadcast(State#state.clients, Username ++ " joined the chat"),
                    HistoryToSend = lists:sublist(State#state.history, max(1, length(State#state.history) - State#state.max_history + 1)),
                    Pid ! {chat_history, HistoryToSend},
                    {reply, {ok, "Connected"}, State#state{clients = NewClients}}
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
            broadcast(NewClients, Username ++ " left the chat"),
            {noreply, State#state{clients = NewClients}};
        error -> {noreply, State}
    end;

handle_cast({message, Username, Message}, State) ->
    FullMessage = "[" ++ Username ++ "]: " ++ Message,
    NewHistory = [FullMessage | lists:sublist(State#state.history, max(1, length(State#state.history) - State#state.max_history + 1))],
    broadcast(State#state.clients, FullMessage),
    {noreply, State#state{history = NewHistory}};

handle_cast({private_message, FromUser, ToUser, Message}, State) ->
    case maps:find(ToUser, State#state.clients) of
        {ok, ToPid} ->
            ToPid ! {private_message, FromUser, Message},
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
