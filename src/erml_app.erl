%%%===================================================================
%%% @doc draft.
%%% @end
%%%===================================================================
-module(erml_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erml_sup:start_link().

stop(_State) ->
    ok.

