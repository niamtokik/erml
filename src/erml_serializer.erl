%%%===================================================================
%%% @doc draft.
%%% @end
%%%===================================================================
-module(erml_serializer).
-export([compile/3, compile/4]).
-export([flatten/2]).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc serialize an erml template.
%%
%% @end
%%--------------------------------------------------------------------
compile(Module, Args, Data) ->
    compile(Module, Args, Data, #{}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
compile(Module, Args, Data, Opts) ->
    init_loop(Module, Args, Data, Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init_loop(Module, Args, Data, Opts) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init_loop, [Module, Args, Data, Opts]}]),
    try Module:init(Args) of
        {ok, State} ->
            loop(Module, Data, Opts, State, [], [])
    catch
        E:R:S ->
            {error, {E,R,S}}
    end.

%%--------------------------------------------------------------------
%% custom function to flatten an improper list containing binaries and
%% other terms.
%%--------------------------------------------------------------------
flatten([], Buffer)
  when is_binary(Buffer) -> Buffer;
flatten([], Buffer)
  when is_list(Buffer) -> lists:reverse(Buffer);
flatten([H|T], Buffer)
  when is_binary(H), is_binary(Buffer) ->
    flatten(T, <<Buffer/binary, H/binary>>);
flatten([H|T], Buffer)
  when is_binary(Buffer) ->
    flatten(T, [H|[Buffer]]);
flatten([H|T], [Last|Rest])
  when is_binary(Last), is_binary(H) ->
    flatten(T, [<<Last/binary, H/binary>>|Rest]);
flatten([H|T], Buffer)
  when is_list(Buffer) ->
    flatten(T, [H|Buffer]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
loop(Module, Data, Opts, State, LBuffer, RBuffer) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, loop, [Module, Data, Opts, State, LBuffer, RBuffer]}]),
    case elements(Module, Data, Opts, State, LBuffer, RBuffer) of
        {ok, Buffer, _State} ->
            List = lists:flatten(Buffer),
            Flatten = flatten(List, <<>>),
            {ok, Flatten};
        Elsewise ->
            Elsewise
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
elements(Module, [], Opts, State, [], RB) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, elements, [Module, [], Opts, State, [], RB]}]),
    {ok, RB, State};
elements(Module, [], Opts, State, LB, []) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, elements, [Module, [], Opts, State, LB, []]}]),
    {ok, LB, State};
elements(Module, [], Opts, State, LB, RB) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, elements, [Module, [], Opts, State, LB, RB]}]),
    {ok, [LB|RB], State};
elements(Module, [Element|Elements], Opts, State, LB, RB) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, elements, [Module, [Element|Elements], Opts, State, LB, RB]}]),
    case element(Module, Element, Opts, State) of
        {ok, Content, NewState} ->
            elements(Module, Elements, Opts, NewState, [LB, Content], [RB]);
        {ok, Begin, End, NewState} ->
            elements(Module, Elements, Opts, NewState, [LB, Begin, End], [RB]);
        Elsewise ->
            Elsewise
    end;
elements(Module, Element, Opts, State, LB, RB) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, elements, [Module, Element, Opts, State, LB, RB]}]),
    case element(Module, Element, Opts, State) of
        {ok, Content, NewState} ->
            {ok, [LB,Content,RB], NewState};
        {ok, Begin, End, NewState} ->
            {ok, [LB,Begin,End,RB], NewState};
        Elsewise ->
            Elsewise
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
element(Module, Element, Opts, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, element, [Module, Element, Opts, State]}]),
    case Module:tag(Element, Opts, State) of
        {ok, Content, NewState} ->
            {ok, Content, NewState};
        {ok, Begin, End, NewState} ->
            {ok, [Begin, End], NewState};
        {ok, Begin, End, Inner, NewState} ->
            elements(Module, Inner, Opts, NewState, [Begin], [End]);
        Elsewise ->
            Elsewise
    end.
