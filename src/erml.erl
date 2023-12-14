%%%===================================================================
%%% @doc
%%%
%%% @end
%%%===================================================================
-module(erml).
-export([read/1, read/2]).
-export([compile/1, compile/2]).
-export([params/0]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
params() ->
    Root = code:priv_dir(erml),
    #{ root => Root }.

%%--------------------------------------------------------------------
%% @doc Read a file template.
%% @see read/2
%% @end
%%--------------------------------------------------------------------
read(File) ->
    read(File, params()).

%%--------------------------------------------------------------------
%% @doc Read a file template.
%% @end
%%--------------------------------------------------------------------
read(File, Opts) ->
    Merged = maps:merge(params(), Opts),
    case file:consult(File) of
        {ok, Content} ->
            compile(Content, Merged);
        Elsewise ->
            Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc Compile a template.
%% @see compile/2
%% @end
%%--------------------------------------------------------------------
compile(Template) ->
    compile(Template, params()).

%%--------------------------------------------------------------------
%% @doc Compile a template
%% @end
%%--------------------------------------------------------------------
compile(Template, #{ output := list } = Opts) ->
    Merged = maps:merge(params(), Opts),
    case erml_html5:compile(Template, Merged) of
        {ok, Data} when is_binary(Data) ->
            {ok, binary_to_list(Data)};
        Elsewise ->
            Elsewise
    end;
compile(Template, Opts) ->
    Merged = maps:merge(params(), Opts),
    erml_html5:compile(Template, Merged).
