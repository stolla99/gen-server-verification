-module(gen_server_eqc_callbacks).

-compile([
    export_all,
    nowarn_export_all
]).

init([]) ->
    {ok, 0};
init([Num]) ->
    {ok, Num};
init([caller, Pid, Parent]) ->
    {ok, [Pid, Parent]}.

handle_call({increment_reply}, _From, State) ->
    NewState = State + 1,
    {reply, NewState, NewState};
handle_call({increment}, _From, State) ->
    {reply, ok, State + 1};
handle_call({step}, _From, State) ->
    {reply, ok, (State * State) rem (2 + rand:uniform(1022 + 1) - 1)};
handle_call({call_increment}, _From, State = [Pid, Parent]) ->
    ok = gen_server_behaviour_simple:call(Pid, {increment}),
    Parent ! ok,
    {reply, ok, State};
handle_call({cast_increment}, _From, State = [Pid, Parent]) ->
    ok = gen_server_behaviour_simple:cast(Pid, {increment}),
    Parent ! ok,
    {reply, ok, State};
handle_call({info_increment}, _From, State = [Pid, Parent]) ->
    Pid ! {increment},
    Parent ! ok,
    {reply, ok, State};
handle_call(_, _From, State) -> 
    io:format("Invalid operation received in gen_server_eqc_callbacks:handle_call~n"),
    {stop, invalid_operation, State}.

handle_cast({increment}, State) ->
    {noreply, State + 1};
handle_cast(_, State) ->
    {stop, invalid_operation, State}.

handle_info({increment}, State) ->
    {noreply, State + 1};
handle_info(_, State) ->
    {stop, invalid_operation, State}.