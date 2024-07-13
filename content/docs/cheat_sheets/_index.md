+++
label = "Index"
author = ["Fred Hebert", "Tristan Sloughter"]
description = "Adopting Erlang."
date = 2019-08-08T08:06:00+00:00
keywords = ["erlang"]
lastmod = 2024-07-13T10:52:10+00:00
draft = false
title = "Appendix 1: Erlang/OTP Cheat Sheets"
[menu]
  [menu.main]
    identifier = "index"
    weight = 5001
+++

<header>
  <h1>Cheat Sheets</h1>
  <h5>
    <strong>April 28, 2021</strong>
  </h5>
</header>

This section contains various reminders to jog your memory if you're not too fresh on basic Erlang data, types, or syntax.


## Data Types {#data-types}

| Name      | Description                                       | Dialyzer                                                 | Example Syntax                                                                                    |
|-----------|---------------------------------------------------|----------------------------------------------------------|---------------------------------------------------------------------------------------------------|
| integer   | number without decimals                           | `integer()`, `pos_integer()`, `non_neg_integer()`        | `1`, `2`, `3`, `-213`, `16#01FF`, `2#101011`                                                      |
| float     | number with decimals                              | `float()`                                                | `1.0`, `-1.0`, `123.12`, `1.0e232`                                                                |
| number    | either floats or integers                         | `number()`                                               | `1.0`, `1`                                                                                        |
| atom      | literals, constants with their own name for value | `atom()`                                                 | `abc`, `'abc'`, `some_atom@erlang`, `'atom with spaces'`                                          |
| boolean   | atoms `true` or `false`                           | `boolean()`                                              | `true`, `false`                                                                                   |
| reference | unique opaque value                               | `reference()`                                            | `make_ref()`                                                                                      |
| fun       | anonymous function                                | `fun()`, `fun((ArgType) -> RetType)`                     | &lt;code&gt;fun(X) -&gt; X end, fun F(0) -&gt; []; F(N) -&gt; [1 &vert; F(N-1)] end&lt;/code&gt;  |
| port      | opaque type for a file descriptor                 | `port()`                                                 | N/A                                                                                               |
| pid       | process identifier                                | `pid()`                                                  | `<0.213.0>`                                                                                       |
| tuple     | group a known set of elements                     | `tuple()`, `{A, B, C}`                                   | `{celsius, 42`}, `{a, b, c}`, `{ok, {X, Y}}`                                                      |
| map       | a dictionary of terms                             | `map()`, `#{KType => VType}`, `#{specific_key := VType}` | `#{a => b, c => d}`, `Existing#{key := Updated}`                                                  |
| nil       | an empty list                                     | `[]`                                                     | `[]`                                                                                              |
| list      | recursive structure for a list of terms           | `list()`, `[Type]`                                       | `[a, b, c]`, &lt;code&gt;[a &vert; [b &vert; [c &vert; []]]]&lt;/code&gt;, `"a string is a list"` |
| binary    | a flat byte sequence                              | `binary()`                                               | `<<1,2,3,4>>`, `<<"a string can be a binary">>`, `<<X:Size/type, _Rest/binary>>`                  |

Term ordering: `number < atom < reference < fun < port < pid < tuple < map < nil < list < binary`


## Modules and Syntax {#modules-and-syntax}

<a id="code-snippet--all-syntax-mod"></a>
```erlang
%%% This is a module-level comment
%%% @doc This tag includes officiel EDoc documentation.
%%% It can be useful for people to consule
%%% @end
%%% Generate documentation with rebar3 edoc

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Let's start with Module Attributes %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is an attribute or function-specific comment
%% attributes start with a `-', functions with letters.
%% This file should be saved as `sample.erl'
-module(sample).

%% Functions are described in the form Name/Arity, and must
%% be exported through an `-export([...]).' module attribute
-export([f/0, f/1]).
-export([x/0]).         % multiple export attributes can exist

%% You can "import" functions from another module, but
%% for clarity's sake (and because there's no namespaces)
%% nobody really does that
-import(module, [y/0]).

%% .hrl files contain headers, and are imported directly
%% within the module.
%% The following includes a private header file from src/
%% or a public header file from include/ in the current app
-include("some_file.hrl").
%% The following includes a public header file from the
%% include/ file of another application
-include_lib("appname/include/some_file.hrl").

%% specify an interface you implement:
-behaviour(gen_server).

%% Define a record (a tuple that compilers handles in a
%% special way)
-record(struct, {key = default :: term(),
                 other_key     :: undefined | integer()}).

%% Just C-style macros
-define(VALUE, 42).        % ?VALUE in this module becomes `42'
-define(SQUARE(X), (X*X)). % function macro
-define(DBG(Call),         % a fancy debug macro: ?DBG(2 + 2)
        io:format("DBG: ~s (~p): ~p~n",
                  [??Call, {?MODULE, ?LINE}, Call])).

%% Conditionals
-ifdef(MACRO_NAME).        % opposite: -ifndef(MACRO_NAME).
-define(OTHER_MACRO, ok).
-else.                     % other option: -elif(NAME).
-define(MACRO_NAME, ok).
-endif.

%% Type definitions
-type my_type() :: number() | boolean().
-type my_container(T) :: {[T], [T], my_type(), mod:type()}
-export_type([my_type/0, my_container/1]).

%% you can also define custom attributes:
-my_attribute(hello_there).
-author("Duke Erlington").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% And now modules for code and functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc A function with 0 arguments returning an atom
-spec f() -> term(). % optional spec
f() -> ok.

-spec f(number()) -> float().
f(N) -> N + 1.0.

%% Pattern matching with clauses
x([]) -> [];  % base recursive clause for a list
x([_H|T] -> [x | T]. % replace list element with `x' atom

%% @private variable binding rules
same_list(X = [_|_], X) -> true;
same_list([], []) -> true;
same_list(_, _) -> false.

%% Operators in the language
operators(X, Y) ->
    +X, -Y, % unary
    X + Y, X - Y, X * Y, X / Y,   % any numbers
    X div Y, X rem Y,             % integers-only
    X band Y, X bor Y, X bxor Y,  % binary operators
    X bsl Y, X bsr L,             % bit shifting
    not X,                        % boolean not
    X andalso Y, X orelse Y,      % shortcircuit boolean operators
    X < Y, X > Y, X >= Y, X =< Y, % comparison
    X == Y, X /= Y,               % equality (float == int)
    X =:= Y, X =/= Y,             % strict equality (float =/= int)
    X ++ Y, X -- Y,               % append Y to X, delete Y from X
    X ! Y.                        % send message Y to process X

%% Using guards. Valid guard expressions at:
%% erlang.org/doc/reference_manual/expressions.html#guard-sequences
comfortable({celsius, X}) when X >= 18, X =< 26 -> % AND clauses
    true;
comfortable({celsius, _}) ->
    false.

uncomfortable({celsius, X}) when X =< 18; X >= 26 -> % OR clauses
    true;
uncomfortable({celsius, _}) ->
    false.

%% difference with 'andalso' and 'orelse'
conds(X) when (is_number(X) orelse is_integer(X))
               andalso X < 9 ->
    %% equivalent (A AND B) OR C
    true;
conds(X) when is_number(X); is_integer(X), X < 9 ->
    %% - parentheses impossible with , or ;
    %% - equivalent to A OR (B AND C)
    true;
conds(T) when element(1, T) == celsius; is_integer(T) ->
    %% element/2 extracts an element from a tuple. If `T' is
    %% not a tuple, the call fails and `is_integer/1' is tried
    %% instead
    true;
conds(T) when element(1, T) == celsius orelse is_integer(T) ->
    %% this can never work: if element/2 fails, the whole
    %% `orlese' expressoin fails and `is_integer/1' is skipped
    true.

%% Conditionals
conditional('if', Light) ->
    if Light == red -> stop;
       Light == green; Light == yellow -> go_fast;
       true -> burnout % else clause!
    end;
conditional('case', {Light, IsLate}) ->
    case Light of
        green -> go;
        yellow when IsLate -> go_fast;
        _ -> stop
    end;
conditional(pattern, green) -> go;
conditional(pattern, yellow) -> slow;
conditional(pattern, red) -> stop.

%% List and binary comprehensions
comp(ListA, ListB) ->
    [X*X || X <- ListA, X rem 2 == 0], % square even numbers
    [{X,Y} || X <- ListA, Y <- ListB], % all possible pairs
    << <<X:8>> || X <- ListA >>.       % turn list into bytes
comp(BinA, BinB) -> % now with binaries
    << <<X*X:32>> || <<X:8>> <= Bin, X rem 2 == 0 >>,
    [{X,Y} || <<X:32>> <= BinA, <<Y:8>> <= BinB],
    [X || <<X:8>> <= BinA].

%% Anonymous and higher order functions
higher_order() ->
    If = fun(Light) -> conditional('if', Light) end,
    Case = fun(Light) -> conditional('case', {Light, true}) end,
    lists:map(If, [green, yellow, red]),
    lists:map(Case, [green, yellow, red]),
    If(red), % can be called literally
    lists:map(fun(X) -> X*X end, [1,2,3,4,5]).

try_catch() ->
    try
        some_call(),     % exceptions in this call are caught as well
        {ok, val},       % common good return value to pattern match
        {error, reason}, % common bad return value to pattern match
        % any of these expression aborts the execution flow
        throw(reason1), % non-local returns, internal exceptions
        error(reason2), % unfixable error
        exit(reason3)   % the process should terminate
    of  % this section is optional: exceptions here are not caught
        {ok, V} ->
            do_something(V),
            try_catch(); % safely recurse without blowing stack
        {error, R} ->
            {error, R} % just return
    catch % this section is optional: various patterns
        throw:reason1 -> handled;
        reason2 -> oops; % never matches, `throw' is implicit type
        error:reason2 -> handled;
        exit:reason3 -> handled;
        throw:_ -> wildcard_throws;
        E:R when is_error(E) -> any_error;
        _:_:S -> {stacktrace, S}; % extract stacktrace
    after -> % this is an optional 'finally' block
        finally
    end.
```


## Processes and Signals {#processes-and-signals}

<a id="code-snippet--concurrency-constructs"></a>
```erlang
%% Start a new process
Pid = spawn(fun() -> some_loop(Arg) end)
Pid = spawn('name@remote.host', fun() -> some_loop(Arg) end)
Pid = spawn(some_module, some_loop, [Arg])
Pid = spawn('name@remote.host', some_module, some_loop, [Arg])
%% Spawn a linked process
Pid = spawn_link(...) % 1-4 arguments as with spawn/1-4
%% Spawn a monitored process atomically
{Pid, Ref} = spawn_monitor(fun() -> some_loop(Arg) end)
{Pid, Ref} = spawn_monitor(some_module, some_loop, [Arg])
%% Spawn with fancy options
spawn_opt(Fun, Opts)
spawn_opt(Node, Fun, Opts)
spawn_opt(Mod, Fun, Args, Opts)
spawn_opt(Node, Mod, Fun, Args, Opts)
%% Options must respect the following spec; many are advanced
[link | monitor |
 {priority, low | normal | high | max} |    % don't touch
 {fullsweep_after, integer() >= 0} |        % full GC
 {min_heap_size, Words :: integer() >= 0} | % perf tuning
 {min_bin_heap_size, Words} |
 {max_heap_size,                    % heap size after which
   Words |                          % the process may be killed. Use
   #{size => integer() >= 0,        % to indirectly set max queue sizes
     kill => boolean(),
     error_logger => boolean()}}

%% send an exit signal to a process
exit(Pid, Reason)

%% Receive a message
receive
    Pattern1 when OptionalGuard1 ->
        Expression1;
    Pattern2 when OptionalGuard2 ->
        Expression2
after Milliseconds -> % optional
    Expression
end

%% Naming processes
true = register(atom_name, Pid)
true = unregister(atom_name)
Pid | undefined = whereis(atom_name)

%% Monitor
Ref = erlang:monitor(process, Pid)
true = erlang:demonitor(Ref)
true | false = erlang:demonitor(Ref, [flush | info])

%% Links
link(Pid)
unlink(Pid)
process_info(trap_exit, true | false)
```

And the semantics for links and monitors, in diagram forms:

<a id="figure--fig:sig-mon"></a>

{{< figure src="/img/sig_mon_sm.png" caption="<span class=\"figure-number\">Figure 1: </span>Monitors are unidirectional informational signals, and they stack" >}}

<a id="figure--fig:sig-linked-notrap"></a>

{{< figure src="/img/sig_linked_notrap_sm.png" caption="<span class=\"figure-number\">Figure 2: </span>Untrapped links are bidirectional and kill the other process, except if the reason is 'normal'" >}}

<a id="figure--fig:sig-linked-trap"></a>

{{< figure src="/img/sig_linked_trap_sm.png" caption="<span class=\"figure-number\">Figure 3: </span>Trapped links are converted to messages, except for the untrappable 'kill' reason" >}}

OTP processes do have slightly different semantics due to supervision shenanigans:

<a id="figure--fig:sig-otp-notrap"></a>

{{< figure src="/img/sig_otp_notrap_sm.png" caption="<span class=\"figure-number\">Figure 4: </span>Untrapped links work the same for OTP" >}}

<a id="figure--fig:sig-otp-trap"></a>

{{< figure src="/img/sig_otp_trap_sm.png" caption="<span class=\"figure-number\">Figure 5: </span>Trapped links behave in a special way when the parent of a process is the one that dies" >}}

<a id="figure--fig:sig-otp-own"></a>

{{< figure src="/img/sig_otp_own_sm.png" caption="<span class=\"figure-number\">Figure 6: </span>Supervisors log things differently based on the termination reason" >}}


## Behaviours {#behaviours}

Not all OTP behaviours are listed here, only thee most frequently-used ones.


### Applications {#applications}

| Trigger                                       | Called By                | Handled By                                                    | Return                                                         | Description                                                                                            |
|-----------------------------------------------|--------------------------|---------------------------------------------------------------|----------------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| `application:start/1-2`                       | client or booting VM     | `start(Type, Args)`                                           | &lt;code&gt;{ok, pid()} &vert; {ok, pid(), State}&lt;/code&gt; | should start the root supervisor                                                                       |
| `{start_phases, [{Phase, Args}]}` in app file | `kernel` booting the app | `start_phase(Phase, Type, Args)`                              | &lt;code&gt;ok &vert; {error, Reason}&lt;/code&gt;             | Optional. Can isolate specific steps of initialization                                                 |
| `application:stop/1`                          | app shutting down        | `prop_stop(State)`                                            | `State`                                                        | Optional. Called before the supervision tree is shut down                                              |
| `application:stop/1`                          | app shutting down        | `stop(State)`                                                 | `term()`                                                       | called once the app is done running to clean things up                                                 |
| Hot code update                               | SASL's release handler   | `config_change(Changed::[{K,V}], New::[{K,V}], Removed::[K])` | `ok`                                                           | Called after a hot code update using the VM's relup functionality, if the configuration values changed |


### Supervisors {#supervisors}

| Trigger                     | Called By      | Handled By  | Return                                                          | Description                                             |
|-----------------------------|----------------|-------------|-----------------------------------------------------------------|---------------------------------------------------------|
| `supervisor:start_link/2-3` | parent process | `init(Arg)` | &lt;code&gt;ignore &vert; {ok, {SupFlag, [Child]}}&lt;/code&gt; | Specifies a supervisor. Refer to official documentation |


### gen_server {#gen-server}

| Trigger                                        | Called By                 | Handled By                                                                      | Return                                                                                                    | Description                                                                                                                            |
|------------------------------------------------|---------------------------|---------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------|
| `gen_server:start_link/3-4`                    | supervisor                | `init(Arg)`                                                                     | &lt;code&gt;{ok, State [, Option]} &vert; ignore &vert; {stop, Reason}&lt;/code&gt;                       | Set up the initial state of the process                                                                                                |
| `gen_server:call/2-3`                          | client                    | `handle_call(Msg, From, State)`                                                 | &lt;code&gt;{Type::reply &vert; noreply, State [, Option]} &vert; {stop, Reason [, Reply], State}code&gt; | Request/response pattern. A message is received and expects an answer                                                                  |
| `gen_server:cast/2`                            | client                    | `handle_cast(Msg, State)`                                                       | &lt;code&gt;{noreply, State [, Option]} &vert; {stop, Reason, State}&lt;/code&gt;                         | Information sent to the process; fire and forget                                                                                       |
| `Pid ! Msg`                                    | client                    | `handle_info(Msg, State)`                                                       | same as `handle_cast/2`                                                                                   | Out-of-band messages, including monitor signals and `'EXIT'` messages when trappig exit                                                |
| Setting an `Option` value to `{continue, Val}` | the server itself         | `handle_continue(Val, State)`                                                   | same as `handle_cast/2`                                                                                   | Used to break longer operations into triggerable internal events                                                                       |
| `gen_server:stop/1,3`                          | client or supervisor      | `terminate(Reason, State)`                                                      | `term()`                                                                                                  | Called when the process is shutting down willingly or through errors. If the process does not trap exits, this callback may be omitted |
| `sys:get_status/2-3`, crash logs               | client, the server itself | &lt;code&gt;format_status(normal &vert; terminate, [PDict, State])&lt;/code&gt; | `[{data, [{"State", Term}]}]`                                                                             | Used to add or remove information that would make it to debugging calls or error logs                                                  |
| N/A                                            | supervisor                | `code_change(OldVsn, State, Extra)`                                             | `{ok, NewState}`                                                                                          | called to update a stateful process if the proper instructions are given during a hot code upgrade with releases                       |


### gen_statem {#gen-statem}


#### Process management {#process-management}

| Trigger                          | Called By                 | Handled By                                                                            | Return                                                                                     | Description                                                                                                                            |
|----------------------------------|---------------------------|---------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------|
| `gen_statem:start_link/3-4`      | supervisor                | `init(Arg)`                                                                           | &lt;code&gt;{ok, State, Data [, Actions]} &vert; ignore &vert; {stop, Reason}&lt;/code&gt; | Sets the initial state and data for the state machine                                                                                  |
| N/A                              | internal                  | `callback_mode()`                                                                     | &lt;code&gt;[state_functions &vert; handle_event_function [, state_enter]]&lt;/code&gt;    | Defines the type of FSM and whether entering a state triggers a special internal event                                                 |
| `gen_statem:stop/1,3`            | client or supervisor      | `terminate(Reason, State, Data)`                                                      | `term()`                                                                                   | Called when the process is shutting down willingly or through errors. If the process does not trap exits, this callback may be omitted |
| `sys:get_status/2-3`, crash logs | client, the server itself | &lt;code&gt;format_status(normal &vert; terminate, [PDict, State, Data])&lt;/code&gt; | `[{data, [{"State", Term}]}]`                                                              | Used to add or remove information that would make it to debugging calls or error logs                                                  |
| N/A                              | supervisor                | `code_change(OldVsn, State, Data, Extra)`                                             | `{ok, NewState, NewData}`                                                                  | called to update a stateful process if the proper instructions are given during a hot code upgrade with releases                       |


#### State handling and transitions {#state-handling-and-transitions}

Handled by either `handle_event/4` or `StateName/3` functions, based on the value of `callback_mode()`. The function signatures are either:

-   `handle_event(EventType, EventDetails, State, Data)`
-   `State(EventType, EventDetails, Data)`

If the value of `State` is not a list, even though `callback_mode()` defined `state_functions`, then `handle_event/4` will be called. All possible return values for either functions are one of:

-   `{next_state, State, Data}`
-   `{next_state, State, Data, [Actions, ...]}`
-   `{stop, Reason, Data}`
-   `{stop, Reason, Data, [Actions, ...]}`

Various short forms exist, such as `keep_state_and_data`, `{keep_state, Data}`, `{repeat_state, Data}`, and many more. Refer to the documentation for their content.

The `Actions` value is any combination of the following list (non-inclusive): `postpone`, `{next_event, EventType, EventDetails}`, `hibernate`, `{timeout, Delay, EventDetails}`, `{state_timeout, Delay, EventDetails}`, `{reply, From, Reply}`, `hibernate`. Consult the documentation for more options.

| Trigger                       | Called By             | Event Type      | Event Details | Description                                                                                                                                         |
|-------------------------------|-----------------------|-----------------|---------------|-----------------------------------------------------------------------------------------------------------------------------------------------------|
| `gen_statem:call/2-3`         | client                | `{call, From}`  | `term()`      | Request/response pattern. A message is received and is expected to receive an answer                                                                |
| `gen_statem:cast/2`           | client                | `cast`          | `term()`      | Information must be sent to the process; fire and forget                                                                                            |
| Pid ! Msg                     | client                | `info`          | `Msg`         | Out-of-band messages, including monitor messages and `'EXIT'` signals that are trapped                                                              |
| `{timeout, T, Msg}`           | `Action` return value | `timeout`       | `Msg`         | A specific timeout that can be set and received internally when the state machine has not received a new event in `T` milliseconds                  |
| `{state_timeout, T, Msg}`     | `Action` return value | `state_timeout` | `Msg`         | A specific timeout that can be set and received internally when the state machine has not transitioned to a new different state in `T` milliseconds |
| `{next_event, internal, Msg}` | `Action` return value | `internal`      | `Msg`         | Internal messages that can be generated by a state machine wanting to trigger itself without looking like external calls                            |

<div class="pagination">
  <div><a href="/docs/team_building">← Prev</a></div>
</div>
