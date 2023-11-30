%%%===================================================================
%%% @doc
%%% see: https://rebar3.org/docs/extending/custom_compiler_plugins/
%%% @end
%%%===================================================================
-module(rebar3_erml_compiler).
-export([init/1, do/1, format_error/1]).

init(State) ->
    Opts = [{name, compile},
            {namespace, erml},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, app_discovery}]},
            {example, "rebar3 erml compile"},
            {opts, []},
            {short_desc, "Erml file compiler"},
            {desc, ""}
           ],
    Provider = providers:create(Opts),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    {ok, State}.

format_error(Message) ->
    ok.
