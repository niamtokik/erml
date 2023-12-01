%%%===================================================================
%%% @doc draft module to convert erml html into behavior.
%%%
%%% The idea behind this module is to offer a comprehensive, flexible
%%% and easy interface to generate optimized HTML code. When
%%% evaluated, a template should produce two kind of data:
%%%
%%%   1. if the page is static, a `binary' term is produced containing
%%%      the whole document
%%%
%%%   2. if the page is dynamic (including some function call), a list
%%%      containing binary and function is then produced.
%%%
%%% == Requirement ==
%%%
%%%   - Reusable HTML part
%%%
%%%   - Optimized HTML
%%%
%%%   - Flexible HTML generator (include, function, module call...)
%%%
%%%   - Flexible HTML entities (standard, strict, custom...)
%%%
%%%   - Compatible with spectre css by default (see:
%%%     https://picturepan2.github.io/spectre/)
%%%
%%%   - Compatible with htmx by default (see: https://htmx.org/)
%%%
%%% == More ==
%%%
%%% More tests will be required to evaluate the performance of the
%%% whole implementation, and if it's required to create dedicated
%%% module when compiling. Different models can be used:
%%%
%%%   - Store templates into ETS (in memory)
%%%
%%%   - Compile a module with common interface
%%%
%%%   - Store static page in cache with versionning and dynamic one on
%%%     another storage layer.
%%%
%%% == Notes ==
%%%
%%% ```
%%% {}: null
%%% {Variable}: variable
%%% {Tag, Content}: tag without attribute
%%% {Tag, Attribute, Content}: full tag
%%% {Tag, Attribute, Content, Opts}: full tag with custom options
%%% {apply, Module, Function, Args}: special tag
%%% {apply, Function}
%%% {apply, Function, Args}
%%% '''
%%%
%%% Static template produce (a binary):
%%%
%%% ```
%%% <<"<body><head>test</head></body>">>
%%% '''
%%%
%%% Dynamic template production (a list of binaries and tuples):
%%%
%%% ```
%%% [ <<"<body><head>"
%%% , {apply, Module, Function, Args}
%%% , "</head></body>">>
%%% ]
%%% '''
%%%
%%% ```
%%% 1: [{body, #{}, {head, #{}, []}}].
%%% 2: {body, #{}, {head, #{}, []}}.
%%% 3: ["<body>", {head, #{}, []} ,"</body>"]
%%% 4: ["<body>", "<head>", "</head>", "</body>"]
%%% 5: "<body><head></head></body>"
%%% '''
%%%
%%% @end
%%%===================================================================
-module(erml_tag).
-export([create/1, create/2]).
-export([init/1]).
-export([tag/3]).
-export([attribute/5]).
-export([content/3]).

%%--------------------------------------------------------------------
%% some type definition.
%%--------------------------------------------------------------------
-type state() :: term().
-type data() :: binary() | function().
-type return_ok() :: {ok, data(), state()}.
-type return_stop() :: {stop, term(), state()}.
-type return() :: return_ok() | return_stop().

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
create(Data) -> create(Data, #{}).
create(Data, Opts) -> erml_serializer:compile(?MODULE, [], Data, Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: term(),
      Return :: term().

init(Args) ->
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc main function used to generate html using erml tags.
%% @end
%%--------------------------------------------------------------------
-spec tag(Tag, Opts, State) -> Return when
      Tag :: term(),
      Opts :: map(),
      State :: term(),
      Return :: return().

%---------------------------------------------------------------------
% add variable support (template only). A variable can be any Erlang
% term but MUST be present in variables key from Opts. When using a
% variable as template, one can't create another variable, it could
% create an infinite loop, that's why the state changes and is tagged
% with `variable_loop' atom.
%---------------------------------------------------------------------
tag({Variable}, #{variables := Variables} = Opts, State) ->
    tag_variable(Variable, Opts, State);
tag({_Variable}, Opts, State) ->
    {stop, {not_configured, variables, Opts}, State};

%--------------------------------------------------------------------
% explicit content with specific options
%--------------------------------------------------------------------
tag({content, Content} = Tag, Opts, State) ->
    content(Content, Opts, State);
tag({content, Content, LocalOpts} = Tag, Opts, State) ->
    content(Content, maps:merge(Opts, LocalOpts), State);

%---------------------------------------------------------------------
% add raw include support. A raw include can use default parameters
% from Opts but can also use local parameters, useful if someone needs
% to insert a script or stylecheat.
%---------------------------------------------------------------------
tag({include_raw, Path}, Opts, State) ->
    tag({include_raw, Path, Opts}, Opts, State);
tag({include_raw, Path, LocalOpts} = Tag, Opts, State) ->
    Options = maps:merge(Opts, LocalOpts),
    {stop, {todo, Tag, Opts}, State};

%---------------------------------------------------------------------
% add template include support. A template is an erml file or a list
% of term containing erml html tags.
%---------------------------------------------------------------------
tag({include_template, Path} = Tag, Opts, State) ->
    {stop, {todo, Tag, Opts}, State};
tag({include_template, Path, LocalOpts} = Tag, Opts, State) ->
    {stop, {todo, Tag, Opts}, State};

%---------------------------------------------------------------------
% add template include support. A template is an erml file or a list
% of term containing erml html tags.
%---------------------------------------------------------------------
tag({include_media, Path} = Tag, Opts, State) ->
    {stop, {todo, Tag, Opts}, State};
tag({include_media, Path, LocalOpts} = Tag, Opts, State) ->
    {stop, {todo, Tag, Opts}, State};

%---------------------------------------------------------------------
% call a standard behaviors and integrate their answer in the
% template. this will create a dynamic template. This part of the code
% is managed by the generator or the final renderer. 
%---------------------------------------------------------------------
% default call  to gen_server
tag({call, Pid, Message}, Opts, State) ->
    tag({gen_server, call, Pid, Message, 1000}, Opts, State);
tag({call, Pid, Message, Timeout}, Opts, State) ->
    tag({gen_server, call, Pid, Message, Timeout}, Opts, State);

% gen_server support
tag({gen_server, call, Pid, Message}, Opts, State) ->
    tag({gen_server, call, Pid, Message, 1000}, Opts, State);
tag({gen_server, call, Pid, Message, Timeout} = Call, Opts, State) ->
    {ok, Call, State};

% statem support
tag({gen_statem, call, Pid, Message}, Opts, State) ->
    tag({gen_statem, call, Pid, Message, 1000}, Opts, State);
tag({gen_statem, call, Pid, Message, Timeout} = Call, Opts, State) ->
    {ok, Call, State};

%---------------------------------------------------------------------
% add MFA support. This part of the code add a dynamic layer to the
% page. Every time the page is called, MFA defined is called and then
% generate a new page.
%---------------------------------------------------------------------
tag({apply, Module, Function, Arguments}, Opts, State) 
  when is_atom(Module), is_atom(Function), is_list(Arguments) ->
    try apply(Module, Function, Arguments) of
        {ok, Result} when is_binary(Result) ->
            {ok, Result, State}
    catch
        E:R:S -> {E,R,S}
    end;

%---------------------------------------------------------------------
% add local function support (helper) to execute on the same module.
%---------------------------------------------------------------------
tag({apply, Function, Arguments}, Opts, State) 
  when is_atom(Function), is_list(Arguments) ->
    try Function(Arguments) of
        {ok, Result} when is_binary(Result) ->
            {ok, Result, State}
    catch
        E:R:S -> {E,R,S}
    end;

%---------------------------------------------------------------------
% add support for function generator (without argument). Same as MFA.
%---------------------------------------------------------------------
tag({apply, Function}, Opts, State)
  when is_function(Function, 0) ->
    try Function() of
        {ok, Result} when is_binary(Result) ->
            {ok, Result, State}
    catch
        E:R:S -> {E,R,S}
    end;

%---------------------------------------------------------------------
% add support for function with opts. Same as MFA.
%---------------------------------------------------------------------
tag({apply, Function}, Opts, State)
  when is_function(Function, 1) ->
    try Function(Opts) of
        {ok, Result} when is_binary(Result) ->
            {ok, Result, State}
    catch
        E:R:S -> {E,R,S}
    end;

%---------------------------------------------------------------------
% add support for empty tag. Empty tags are special tags without
% content.
%---------------------------------------------------------------------
tag({empty, Tag} = T, Opts, State) ->
    {stop, {todo, T, Opts}, State};
tag({{empty, Tag} = T, Attributes}, Opts, State) ->
    {stop, {todo, T, Opts}, State};

%--------------------------------------------------------------------
% some tags will behave differently:
%  - base
%  - br
%  - img
%  - input
%  - link
%  - meta
%  - source
%--------------------------------------------------------------------
tag({<<"code">>, Attributes, [Integer|_] = Content} = Tag, Opts, State)
  when is_list(Content), is_integer(Integer), Integer>0 ->
    Result = list_to_binary(Content),
    tag({<<"code">>, Attributes, Result}, Opts, State);
tag({<<"code">>, Attributes, [List|_] = Content} = Tag, Opts, State)
  when is_list(Content), is_list(List) ->
    Result = list_to_binary(string:join(Content, "\n")),
    tag({<<"code">>, Attributes, Result}, Opts, State);
tag({<<"code">>, Attributes, [Binary|_] = Content} = Tag, Opts, State)
  when is_list(Content), is_binary(Binary) ->
    Result = join(Content, <<"\n">>),
    tag({<<"code">>, Attributes, Result}, Opts, State);
tag({<<"code">>, Attributes, Content}, Opts, State) 
  when is_binary(Content) ->
    Begin = bracket(<<"code">>, <<>>),
    End = bracket_end(<<"code">>),
    {ok, <<Begin/binary, Content/binary, End/binary>>, State};

tag({<<"pre">>, Attributes, [Integer|_] = Content} = Tag, Opts, State)
  when is_list(Content), is_integer(Integer), Integer>0 ->
    Result = list_to_binary(Content),
    tag({<<"pre">>, Attributes, Result}, Opts, State);
tag({<<"pre">>, Attributes, [List|_] = Content} = Tag, Opts, State)
  when is_list(Content), is_list(List) ->
    Result = list_to_binary(string:join(Content, "\n")),
    tag({<<"pre">>, Attributes, Result}, Opts, State);
tag({<<"pre">>, Attributes, [Binary|_] = Content} = Tag, Opts, State)
  when is_list(Content), is_binary(Binary) ->
    Result = join(Content, <<"\n">>),
    tag({<<"pre">>, Attributes, Result}, Opts, State);
tag({<<"pre">>, Attributes, Content}, Opts, State) 
  when is_binary(Content) ->
    Begin = bracket(<<"pre">>, <<>>),
    End = bracket_end(<<"pre">>),
    {ok, <<Begin/binary, Content/binary, End/binary>>, State};

%---------------------------------------------------------------------
% add support for regular tag
%---------------------------------------------------------------------
tag({Tag, Content}, Opts, State) ->
    tag({Tag, #{}, Content}, Opts, State);
tag({Tag, Attributes, Content}, Opts, State)
  when is_binary(Tag) ->
    case attributes(Tag, Attributes, Opts, State) of
        {ok, <<>>, NewState} ->
            Begin = bracket(Tag, <<>>),
            End = bracket_end(Tag),
            {ok, Begin, End, Content, State};
        {ok, Serialized, NewState} ->
            Begin = bracket(Tag, Serialized),
            End = bracket_end(Tag),
            {ok, Begin, End, Content, State}
    end;

%---------------------------------------------------------------------
% add support when a list is present. If a list is present, we assume
% this is a list of tags and we should treat them one by one.
%---------------------------------------------------------------------
tag(Tags, Opts, State)
  when is_list(Tags) ->
    {stop, {todo, Tags, Opts}, State};

%---------------------------------------------------------------------
% All tags defined as atom, list or numbers are converted into binary
% by default. It offers a flexible way to define tags for the developer.
%---------------------------------------------------------------------
tag({Tag, Content}, Opts, State) ->
    tag({Tag, #{}, Content}, Opts, State);
tag({Tag, Attributes, Content}, Opts, State)
  when is_atom(Tag) ->
    NewTag = atom_to_binary(Tag),
    tag({NewTag, Attributes, Content}, Opts, State);
tag({Tag, Attributes, Content}, Opts, State)
  when is_list(Tag) ->
    NewTag = list_to_binary(Tag),
    tag({NewTag, Attributes, Content}, Opts, State);
tag({Tag, Attributes, Content}, Opts, State)
  when is_integer(Tag) ->
    NewTag = integer_to_binary(Tag),
    tag({NewTag, Attributes, Content}, Opts, State);
tag({Tag, Attributes, Content}, Opts, State)
  when is_float(Tag) ->
    NewTag = float_to_binary(Tag),
    tag({NewTag, Attributes, Content}, Opts, State);

%--------------------------------------------------------------------
% we assume binary, numbers, and atoms are text. these tags must be
% protected and encoded with html entities.
%--------------------------------------------------------------------
tag(Text, Opts, State)
  when is_binary(Text); is_atom(Text); 
       is_integer(Text); is_float(Text) ->
    content(Text, Opts, State);

%---------------------------------------------------------------------
% If unsupported tags are present, we should stop.
%---------------------------------------------------------------------
tag(Unsupported, Opts, State) ->
    {stop, {todo, Unsupported, Opts}, State}.

%%--------------------------------------------------------------------
%% internal function to deal with variable.
%%--------------------------------------------------------------------
tag_variable(Variable, Opts, #{variable_recursion := Counter} = State) 
  when Counter>100 ->
    {stop, {variable_recursion, Variable}, State};
tag_variable(Variable, #{variables := Variables} = Opts, State) ->
    Counter = maps:get(variable_recursion, State, 0),
    try #{Variable := Value} = Variables of
        %% _ when is_binary(Value) -> 
        %%     content(Value, Opts, State);
        %% _ when is_atom(Value) -> 
        %%     content(Value, Opts, State);
        %% _ when is_integer(Value) -> 
        %%     content(Value, Opts, State);
        %% _ when is_float(Value) -> 
        %%     content(Value, Opts, State);
        _  -> 
            case tag(Value, Opts, State#{variable_recursion => Counter+1}) of
                {ok, Begin, End, Content, NewState} ->
                    {ok, Begin, End, Content, NewState};
                Elsewise ->
                    Elsewise
            end;
        _ -> {stop, {unsupported, Variable}, State}
    catch
        _:_ -> {stop, {not_found, Variable}, State}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
attributes(Tag, Attributes, Opts, State) ->
    attributes(Tag, Attributes, maps:keys(Attributes), [], Opts, State).

attributes(Tag, Attributes, [], Buffer, Opts, State) ->
    {ok, join(lists:reverse(Buffer)), State};
attributes(Tag, Attributes, [Key|Keys], Buffer, Opts, State) ->
    Value = maps:get(Key, Attributes),
    case attribute(Tag, Key, Value, Opts, State) of
        {ok, K, V, NewState} ->
            Pair = <<K/binary,"=",V/binary>>,
            attributes(Tag, Attributes, Keys, [Pair|Buffer], Opts, NewState);
        Elsewise ->
            Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc main function used to generate html using erml tags.
%% @end
%%--------------------------------------------------------------------
-spec attribute(Tag, Key, Value, Opts, State) -> Return when
      Tag :: binary(),
      Key :: term(),
      Value :: term(),
      Opts :: map(),
      State :: term(),
      Return :: return().

% convert keys to binary
attribute(Tag, Key, Value, Opts, State) 
  when is_atom(Key) ->
    attribute(Tag, atom_to_binary(Key), Value, Opts, State);
attribute(Tag, Key, Value, Opts, State)
  when is_list(Key) ->
    attribute(Tag, list_to_binary(Key), Value, Opts, State);

% convert values to binary
attribute(Tag, Key, Value, Opts, State) 
  when is_atom(Value) ->
    attribute(Tag, Key, atom_to_binary(Value), Opts, State);
attribute(Tag, Key, Value, Opts, State) 
  when is_list(Value) ->
    attribute(Tag, Key, list_to_binary(Value), Opts, State);

% some example of specific attributes
attribute(<<"a">>, <<"href">> = Key, Value, Opts, State) ->
    {ok, Key, Value, State};
attribute(<<"img">>, <<"src">> = Key, Value, Opts, State) ->
    {ok, Key, Value, State};
attribute(_Tag, <<"style">>, Value, Opts, State) ->
    {ok, <<"style">>, Value, State};
attribute(_Tag, <<"on", _/binary>> = Key, Value, Opts, State) ->
    {ok, Key, Value, State};
attribute(_Tag, <<"hx-vals">> = Key, Value, Opts, State) ->
    {ok, Key, Value, State};
attribute(_Tag, <<"hx-", _binary/binary>> = Key, Value, Opts, State) ->
    {ok, Key, Value, State};
attribute(_Tag, <<"htmx-", _binary/binary>> = Key, Value, Opts, State) ->
    {ok, Key, Value, State};
attribute(_Tag, Key, Value, Opts, State)
  when is_binary(Key), is_binary(Value) ->
    {ok, Key, quote(Value, $"), State};

% global attributes, by default, not supported
attribute(Tag, Key, Value, Opts, State) ->
    {stop, {todo, Tag, Key, Value, Opts, State}, State}.

%%--------------------------------------------------------------------
%% @doc main function to check content (inner text).
%% @end
%%--------------------------------------------------------------------
-spec content(Content, Opts, State) -> Return when
      Content :: term(),
      Opts :: map(),
      State :: term(),
      Return :: return().

content(Content, Opts, State) 
  when is_atom(Content) ->
    content(atom_to_binary(Content), Opts, State);
content(Content, Opts, State) 
  when is_integer(Content) ->
    content(integer_to_binary(Content), Opts, State);
content(Content, Opts, State) 
  when is_float(Content) ->
    content(float_to_binary(Content), Opts, State);
content(Content, Opts, State) 
  when is_list(Content) ->
    content(list_to_binary(Content), Opts, State);
content(Content, #{ entities := false } = _Opts, State)
  when is_binary(Content) ->
    {ok, Content, State};
content(Content, Opts, State)
  when is_binary(Content) ->
    {ok, erml_html_entities:encode(Content), State};
content(Content, Opts, State) ->
    {stop, {todo, content, Content, Opts, State}, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
join([]) -> <<>>;
join(Binaries) -> join(Binaries, <<" ">>).

join([], _) -> <<>>;
join(Binaries, Sep) -> join(Binaries, Sep, <<>>).

join([], _, Buffer) -> Buffer;
join([Binary], _, Buffer) ->
    <<Binary/binary, Buffer/binary>>;
join([Binary|Rest], Sep, Buffer) ->
    join(Rest, Sep, <<Sep/binary, Binary/binary, Buffer/binary>>).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
quote(Value, Char) ->
    <<Char, Value/binary, Char>>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
bracket_empty(Element, <<>>) ->
    <<"<", Element/binary, " />">>;
bracket_empty(Element, Attributes) ->
    <<"<", Element/binary, " ", Attributes/binary, " />">>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
bracket(Element, <<>>) ->
    <<"<", Element/binary, ">">>;
bracket(Element, Attributes) ->
    <<"<", Element/binary, " ", Attributes/binary, ">">>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
bracket_end(Element) ->
    <<"</", Element/binary, ">">>.
