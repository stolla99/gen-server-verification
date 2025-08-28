-module(util).

-export([
    hasKey/2, isRecord/1, 
    isAtom/1, isSeq/1, isTuple/1, isInt/1, isString/1, isTimeout/1, isExported/3,
    init_it/6, 
    terminate/10, terminate/8, terminate/9, 
    continue/8, system_get_state/1,
    list_to_tuple/1, tuple_to_list/1,
    enter_loop/4, enter_loop/3, enter_loop/5,
    getNode/0, getNodes/0]).
-export([try_terminate/3]).

hasKey(Key, Map) -> 
    maps:is_key(Key, Map).

isExported(Mod, Fun, Arity) ->
    erlang:function_exported(Mod, Fun, Arity).

isRecord(Map) ->
    is_map(Map).

isSeq(List) ->
    is_list(List).

isInt(Int) ->
    is_integer(Int).

isAtom(Atom) ->
    is_atom(Atom).

isTuple(Tuple) ->
    is_tuple(Tuple).

isTimeout(Timeout) ->
    is_integer(Timeout) andalso Timeout >= 0.

isString(String) ->
    is_list(String) andalso lists:all(fun(C) -> is_integer(C) andalso C >= 0 andalso C =< 255 end, String).

tuple_to_list(Tuple) ->
    case is_tuple(Tuple) of
        true -> 
            erlang:tuple_to_list(Tuple);
        false -> 
            Tuple
    end.

list_to_tuple(List) ->
    case is_list(List) of
        true -> 
            erlang:list_to_tuple(List);
        false -> 
            List
    end.

getNode() -> 
    node().

getNodes() ->
    nodes().

enter_loop(Module, Options, State, Name, Action) -> 
    HibernateAfterTimeout = gen:hibernate_after(Options),
    Timeout = case Action of
        T when is_integer(T) andalso T > 0 -> T;
        _ -> infinity
    end,
    Hibernate = case Action of
        hibernate -> hibernate;
        _ -> undefined
    end,
    Continue = case Action of
        {continue, _} -> Action;
        _ -> undefined
    end,
    RegisteredName = gen:get_proc_name(Name),
    Parent = gen:get_parent(),
    gen_server_behaviour_simple:loop(RegisteredName, Parent, State, Module, HibernateAfterTimeout, Timeout, Hibernate, Continue).

enter_loop(Module, Options, State, Action) -> 
    enter_loop(Module, Options, State, self(), Action).

enter_loop(Module, Options, State) -> 
    HibernateAfterTimeout = gen:hibernate_after(Options),
    Parent = gen:get_parent(),
    gen_server_behaviour_simple:loop(self(), Parent, State, Module, HibernateAfterTimeout, undefined, undefined, undefined).

init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = gen:name(Name0),
    HibernateAfterTimeout = gen:hibernate_after(Options),
    case init_it(Mod, Args) of
	{ok, {ok, State}} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
        %io:format(" *** DBG *** init_it ok,State: ~p, ~p, ~p, ~p, ~p, ~n", [Name, Parent, Mod, Args, Options]),
        gen_server_behaviour_simple:loop(Name, Parent, State, Mod, HibernateAfterTimeout, infinity, undefined, undefined);
    {ok, {ok, State, TimeoutOrHibernate}} when (TimeoutOrHibernate =:= infinity orelse is_integer(TimeoutOrHibernate) andalso TimeoutOrHibernate > 0); TimeoutOrHibernate =:= hibernate ->
	    proc_lib:init_ack(Starter, {ok, self()}),
        Timeout = case TimeoutOrHibernate of
            T when is_integer(T) andalso T > 0 -> T;
            _ -> infinity
        end,
        Hibernate = case TimeoutOrHibernate of
            hibernate -> hibernate;
            _ -> undefined
        end,
        %io:format(" *** DBG *** init_it ok,State,Timeout,Hibernate: ~p, ~p, ~p, ~p, ~p, ~n", [Name, Parent, Mod, Args, Options]),
        gen_server_behaviour_simple:loop(Name, Parent, State, Mod, HibernateAfterTimeout, Timeout, Hibernate, undefined);
	{ok, {ok, State, {continue, _}=Continue}} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
        %io:format(" *** DBG *** init_it ok,State,Continue: ~p, ~p, ~p, ~p, ~p, ~n", [Name, Parent, Mod, Args, Options]),
        gen_server_behaviour_simple:loop(Name, Parent, State, Mod, HibernateAfterTimeout, undefined, undefined, Continue);
	{ok, {stop, Reason}} ->
        %io:format(" *** DBG *** init_it stop,Reason: ~p, ~p, ~p, ~p, ~p, ~n", [Name, Parent, Mod, Args, Options]),
	    gen:unregister_name(Name0),
        exit(Reason);
	{ok, {error, _Reason} = ERROR} ->
        %io:format(" *** DBG *** init_it error,Reason: ~p, ~p, ~p, ~p, ~p, ~n", [Name, Parent, Mod, Args, Options]),
	    gen:unregister_name(Name0),
	    proc_lib:init_fail(Starter, ERROR, {exit, normal});
	{ok, ignore} ->
        %io:format(" *** DBG *** init_it ignore, ~p, ~p, ~p, ~p, ~p, ~n", [Name, Parent, Mod, Args, Options]),
	    gen:unregister_name(Name0),
        proc_lib:init_fail(Starter, ignore, {exit, normal});
	{ok, Else} ->
        %io:format(" *** DBG *** init_it Else, ~p, ~p, ~p, ~p, ~p, ~n", [Name, Parent, Mod, Args, Options]),
	    gen:unregister_name(Name0),
        exit({bad_return_value, Else});
	{'EXIT', Class, Reason, Stacktrace} ->
        %io:format(" *** DBG *** init_it EXIT,Class,Reason,Stacktrace: ~p, ~p, ~p, ~p, ~p, ~n", [Name, Parent, Mod, Args, Options]),
	    gen:unregister_name(Name0),
        erlang:raise(Class, Reason, Stacktrace)
    end.
init_it(Mod, Args) ->
    try
        {ok, Mod:init(Args)}
    catch
        throw:R -> {ok, R};
        Class:R:S -> {'EXIT', Class, R, S}
    end.

continue(Name, Parent, State, Mod, HibernateAfterTimeout, Timeout, Hibernate, Continue) ->
    gen_server_behaviour_simple:loop(Name, Parent, State, Mod, HibernateAfterTimeout, Timeout, Hibernate, Continue).

system_get_state(Args) -> 
    {ok, lists:nth(2, Args)}.

try_terminate(Mod, Reason, State) ->
    case erlang:function_exported(Mod, terminate, 2) of
        true ->
            try
                {ok, Mod:terminate(Reason, State)}
            catch
                throw:R ->
                    {ok, R};
                Class:R:Stacktrace ->
                    {'EXIT', Class, R, Stacktrace}
            end;
        false ->
            {ok, ok}
    end.

terminate(Reason, Stacktrace, Name, From, Msg, Mod, State, Debug) ->
  terminate(exit, Reason, Stacktrace, true, Name, From, Msg, Mod, State, Debug).

terminate(Class, Reason, Stacktrace, Name, From, Msg, Mod, State, Debug) ->
  terminate(Class, Reason, Stacktrace, true, Name, From, Msg, Mod, State, Debug).

catch_result(error, Reason, Stacktrace) -> {Reason, Stacktrace};
catch_result(exit, Reason, _Stacktrace) -> Reason.

terminate(Class, Reason, Stacktrace, ReportStacktrace, _Name, _From, _Msg, Mod, State, _Debug) ->
    Reply = try_terminate(Mod, catch_result(Class, Reason, Stacktrace), State),
    case Reply of
	{'EXIT', C, R, S} ->
	    erlang:raise(C, R, S);
	_ ->
	    case {Class, Reason} of
		{exit, normal} -> ok;
		{exit, shutdown} -> ok;
		{exit, {shutdown,_}} -> ok;
        {exit, stopped} -> ok;
		_ when ReportStacktrace -> ok
	    end
    end,
    case Stacktrace of
	[] ->
	    erlang:Class(Reason);
	_ ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.