%%%===================================================================
%%% @doc
%%%
%%% @end
%%%===================================================================
-module(erml).
-export([read/1, read/2]).
-export([compile/1, compile/2]).

%%--------------------------------------------------------------------
%% @doc Read a file template.
%% @see read/2
%% @end
%%--------------------------------------------------------------------
read(File) ->
    Root = code:priv_dir(erml),
    read(File, #{ root => Root }).

%%--------------------------------------------------------------------
%% @doc Read a file template.
%% @end
%%--------------------------------------------------------------------
read(File, Opts) ->
    case file:consult(File) of
        {ok, Content} ->
            compile(Content, Opts);
        Elsewise ->
            Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc Compile a template.
%% @see compile/2
%% @end
%%--------------------------------------------------------------------
compile(Template) ->
    Root = code:priv_dir(erml),
    compile(Template, #{ root => Root }).

%%--------------------------------------------------------------------
%% @doc Compile a template
%% @end
%%--------------------------------------------------------------------
compile(Template, #{ output := list } = Opts) ->
    case erml_html5:compile(Template, Opts) of
        {ok, Data} when is_binary(Data) ->
            {ok, binary_to_list(Data)};
        Elsewise ->
            Elsewise
    end;
compile(Template, Opts) ->
    erml_html5:compile(Template, Opts).
