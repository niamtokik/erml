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
-module(erml_html5).
-export([compile/1, compile/2]).
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
%% @doc
%% @end
%%--------------------------------------------------------------------
compile(Data) ->
    compile(Data, #{}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
compile(Data, Opts) ->
    erml_serializer:compile(?MODULE, [], Data, Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: term(),
      Return :: term().

init(_Args) ->
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
tag({Variable}, Opts, State) ->
    tag_variable(Variable, Opts, State);

%--------------------------------------------------------------------
% explicit content with specific options
%--------------------------------------------------------------------
tag({content, Content}, Opts, State) ->
    content(Content, Opts, State);
tag({content, Content, LocalOpts}, Opts, State) ->
    content(Content, maps:merge(Opts, LocalOpts), State);

%---------------------------------------------------------------------
% add raw include support. A raw include can use default parameters
% from Opts but can also use local parameters, useful if someone needs
% to insert a script or stylecheat.
%---------------------------------------------------------------------
tag({include_raw, Path}, Opts, State) ->
    tag({include_raw, Path, Opts}, Opts, State);
tag({include_raw, Path, LocalOpts}, #{root := Root} = Opts, State) ->
    CleanRoot = maps:remove(root, LocalOpts),
    CleanOpts = maps:merge(Opts, CleanRoot),
    case include_raw(Path, CleanOpts) of
        {ok, Content} ->
            {ok, Content, State};
        {error, Reason} ->
            {stop, {error, Reason, filename:join(Root, Path)}, State}
    end;
tag({include_raw, _Path, _LocalOpts}, Opts, State) ->
    {stop, {missing_root, Opts}, State};

%---------------------------------------------------------------------
% add template include support. A template is an erml file or a list
% of term containing erml html tags.
%---------------------------------------------------------------------
tag({include_template, Path}, #{ root := Root } = Opts, State) ->
    case include_template(Path, Opts) of
        {ok, Template} ->
            tag(Template, Opts, State);
        {error, Reason} ->
            {stop, {error, Reason, filename:join(Root, Path)}, State}
    end;
tag({include_template, _Path, _LocalOpts}, Opts, State) ->
    {stop, {missing_root, Opts}, State};

%---------------------------------------------------------------------
% add template include support. A template is an erml file or a list
% of term containing erml html tags.
%---------------------------------------------------------------------
% tag({include_media, Path} = Tag, Opts, State) ->
%     {stop, {todo, Tag, Opts}, State};
% tag({include_media, Path, LocalOpts} = Tag, Opts, State) ->
%     {stop, {todo, Tag, Opts}, State};

%--------------------------------------------------------------------
% call a module to create the content of the tag. The module should
% have `init/2' function exposed and should return a valid tag.
%--------------------------------------------------------------------
% tag({include_callback, Module}, Opts, State)
%   when is_atom(Module) ->
%     try apply(Module, init, [Opts, State])
%     catch
%         E:R:S -> {stop, {include_callback, {E,R,S}, State}}
%     end;

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
tag({gen_server, call, _Pid, _Message, _Timeout} = Call, _Opts, State) ->
    {ok, Call, State};

% statem support
tag({gen_statem, call, Pid, Message}, Opts, State) ->
    tag({gen_statem, call, Pid, Message, 1000}, Opts, State);
tag({gen_statem, call, _Pid, _Message, _Timeout} = Call, _Opts, State) ->
    {ok, Call, State};

%---------------------------------------------------------------------
% add MFA support. This part of the code add a dynamic layer to the
% page. Every time the page is called, MFA defined is called and then
% generate a new page.
%---------------------------------------------------------------------
tag({apply, Module, Function, Arguments}, _Opts, State)
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
    try Function(Arguments ++ [Opts]) of
        {ok, Result} when is_binary(Result) ->
            {ok, Result, State}
    catch
        E:R:S -> {E,R,S}
    end;

%---------------------------------------------------------------------
% add support for function generator (without argument). Same as MFA.
%---------------------------------------------------------------------
tag({apply, Function}, _Opts, State)
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
tag({empty, Tag}, Opts, State) ->
    tag({{empty, Tag}, #{}}, Opts, State);

tag({{empty, Tag}, Attributes}, Opts, State)
  when is_binary(Tag) ->
    {ok, bracket_empty(Tag, Attributes, Opts, State), <<>>, State};

tag({{empty, Tag}, Attributes}, Opts, State)
  when is_atom(Tag) ->
    tag({{empty, atom_to_binary(Tag)}, Attributes}, Opts, State);
tag({{empty, Tag}, Attributes}, Opts, State)
  when is_list(Tag) ->
    tag({{empty, list_to_binary(Tag)}, Attributes}, Opts, State);
tag({{empty, Tag}, Attributes}, Opts, State)
  when is_integer(Tag) ->
    tag({{empty, integer_to_binary(Tag)}, Attributes}, Opts, State);
tag({{empty, _Tag} = T, _Attributes}, Opts, State) ->
    {stop, {todo, T, Opts}, State};

%--------------------------------------------------------------------
% empty elements
%--------------------------------------------------------------------
tag({Tag, Attributes}, Opts, State)
  when Tag =:= input; Tag =:= "input"; Tag =:= <<"input">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= area; Tag =:= "area"; Tag =:= <<"area">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= base; Tag =:= "base"; Tag =:= <<"base">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= br; Tag =:= "br"; Tag =:= <<"br">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= col; Tag =:= "col"; Tag =:= <<"col">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= embed; Tag =:= "embed"; Tag =:= <<"embed">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= hr; Tag =:= "hr"; Tag =:= <<"hr">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= img; Tag =:= "img"; Tag =:= <<"img">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= link; Tag =:= "link"; Tag =:= <<"link">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= meta; Tag =:= "meta"; Tag =:= <<"meta">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

tag({Tag, Attributes}, Opts, State)
  when Tag =:= source; Tag =:= "source"; Tag =:= <<"source">> ->
    tag({{empty, Tag}, Attributes}, Opts, State);

%---------------------------------------------------------------------
% special elements
%---------------------------------------------------------------------
% tag({<<"code">>, Attributes, [Integer|_] = Content} = Tag, Opts, State)
%   when is_list(Content), is_integer(Integer), Integer>0 ->
%     Result = list_to_binary(Content),
%     tag({<<"code">>, Attributes, Result}, Opts, State);
% tag({<<"code">>, Attributes, [List|_] = Content} = Tag, Opts, State)
%   when is_list(Content), is_list(List) ->
%     Result = list_to_binary(string:join(Content, "\n")),
%     tag({<<"code">>, Attributes, Result}, Opts, State);
% tag({<<"code">>, Attributes, [Binary|_] = Content} = Tag, Opts, State)
%   when is_list(Content), is_binary(Binary) ->
%     Result = join(Content, <<"\n">>),
%     tag({<<"code">>, Attributes, Result}, Opts, State);
% tag({<<"code">>, Attributes, Content}, Opts, State)
%   when is_binary(Content) ->
%     Begin = bracket(<<"code">>, <<>>),
%     End = bracket_end(<<"code">>),
%     {ok, <<Begin/binary, Content/binary, End/binary>>, State};

% tag({<<"pre">>, Attributes, [Integer|_] = Content} = Tag, Opts, State)
%   when is_list(Content), is_integer(Integer), Integer>0 ->
%     Result = list_to_binary(Content),
%     tag({<<"pre">>, Attributes, Result}, Opts, State);
% tag({<<"pre">>, Attributes, [List|_] = Content} = Tag, Opts, State)
%   when is_list(Content), is_list(List) ->
%     Result = list_to_binary(string:join(Content, "\n")),
%     tag({<<"pre">>, Attributes, Result}, Opts, State);
% tag({<<"pre">>, Attributes, [Binary|_] = Content} = Tag, Opts, State)
%   when is_list(Content), is_binary(Binary) ->
%     Result = join(Content, <<"\n">>),
%     tag({<<"pre">>, Attributes, Result}, Opts, State);
% tag({<<"pre">>, _Attributes, Content}, _Opts, State)
%   when is_binary(Content) ->
%     Begin = bracket(<<"pre">>, <<>>),
%     End = bracket_end(<<"pre">>),
%     {ok, <<Begin/binary, Content/binary, End/binary>>, State};

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
            {ok, Begin, End, Content, NewState};
        {ok, Serialized, NewState} ->
            Begin = bracket(Tag, Serialized),
            End = bracket_end(Tag),
            {ok, Begin, End, Content, NewState}
    end;

%---------------------------------------------------------------------
% add support when a list is present. If a list is present, we assume
% this is a list of tags and we should treat them one by one.
%---------------------------------------------------------------------
tag(List, Opts, State)
  when is_list(List) ->
    case compile(List, Opts) of
        {ok, Result} ->
            {ok, Result, State};
        Elsewise ->
            Elsewise
    end;

%---------------------------------------------------------------------
% All tags defined as atom, list or numbers are converted into binary
% by default. It offers a flexible way to define tags for the developer.
%---------------------------------------------------------------------
% special case
tag({Tag, Content}, Opts, State)
  when is_list(Content) ->
    tag({Tag, #{}, Content}, Opts, State);
tag({Tag, Content}, Opts, State)
  when is_tuple(Content) ->
    tag({Tag, #{}, Content}, Opts, State);

% generic case
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
    {stop, {todo, unsupported, Unsupported, Opts}, State}.

%%--------------------------------------------------------------------
%% internal function to deal with variable.
%%--------------------------------------------------------------------
tag_variable(Variable, _Opts, #{variable_recursion := Counter} = State)
  when Counter>100 ->
    {stop, {variable_recursion, Variable}, State};
tag_variable(Variable, _Opts, #{variable_recursion := Counter} = State)
  when Counter<0 ->
    {stop, {variable_recursion, Variable}, State};
tag_variable(Variable, #{variables := Variables} = Opts, State) ->
    Counter = maps:get(variable_recursion, State, 0),
    try #{Variable := Value} = Variables of
        _ when is_binary(Value) ->
            content(Value, Opts, State);
        _ when is_atom(Value) ->
            content(Value, Opts, State);
        _ when is_integer(Value) ->
            content(Value, Opts, State);
        _ when is_float(Value) ->
            content(Value, Opts, State);
        _  ->
            tag(Value, Opts#{ variables => maps:remove(Variable, Variables)}
               ,State#{variable_recursion => Counter})
    catch
        _:_ -> {stop, {not_found, Variable}, State}
    end;
tag_variable(_Variable, Opts, State) ->
    {stop, {not_configured, variables, Opts}, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc attributes generation.
%% @end
%%--------------------------------------------------------------------
attributes(Tag, Attributes, Opts, State)
  when is_list(Attributes) ->
    attributes_list(Tag, Attributes, [], Opts, State);
attributes(Tag, Attributes, Opts, State)
  when is_map(Attributes) ->
    attributes(Tag, Attributes, maps:keys(Attributes), [], Opts, State).

%%--------------------------------------------------------------------
%% @hidden
%% @doc attributes as map.
%% @end
%%--------------------------------------------------------------------
attributes(_Tag, _Attributes, [], Buffer, _Opts, State) ->
    {ok, join(lists:reverse(Buffer)), State};
attributes(Tag, Attributes, [Key|Keys], Buffer, Opts, State) ->
    Value = maps:get(Key, Attributes),
    case attribute(Tag, Key, Value, Opts, State) of
        {ok, K, <<>>, NewState} ->
            Pair = <<K/binary>>,
            attributes(Tag, Attributes, Keys, [Pair|Buffer], Opts, NewState);
        {ok, K, V, NewState} ->
            Pair = <<K/binary,"=",V/binary>>,
            attributes(Tag, Attributes, Keys, [Pair|Buffer], Opts, NewState);
        Elsewise ->
            Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc attributes as list.
%% @end
%%--------------------------------------------------------------------
attributes_list(_Tag, [], Buffer, _Opts, State) ->
    {ok, join(Buffer), State};
attributes_list(Tag, [Attribute|Attributes], Buffer, Opts, State)
  when is_atom(Attribute) ->
    Void = atom_to_binary(Attribute),
    attributes_list(Tag, Attributes, [Void|Buffer], Opts, State);
attributes_list(Tag, [Attribute|Attributes], Buffer, Opts, State)
  when is_binary(Attribute) ->
    attributes_list(Tag, Attributes, [Attribute|Buffer], Opts, State);
attributes_list(Tag, [Attribute|Attributes], Buffer, Opts, State)
  when is_list(Attribute) ->
    Binary = list_to_binary(Attribute),
    attributes_list(Tag, Attributes, [Binary|Buffer], Opts, State);
attributes_list(Tag, [{Key,Value}|Attributes], Buffer, Opts, State) ->
    case attribute(Tag, Key, Value, Opts, State) of
        {ok, K, V, NewState} ->
            Pair = <<K/binary,"=",V/binary>>,
            attributes_list(Tag, Attributes, [Pair|Buffer], Opts, NewState);
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
attribute(Tag, Key, Value, Opts, State)
  when is_integer(Value) ->
    attribute(Tag, Key, integer_to_binary(Value), Opts, State);
attribute(Tag, Key, Value, Opts, State)
  when is_float(Value) ->
    attribute(Tag, Key, float_to_binary(Value), Opts, State);

% some example of specific attributes
%% attribute(<<"a">>, <<"href">> = Key, Value, Opts, State) ->
%%     {ok, Key, Value, State};
%% % attribute(<<"img">>, <<"src">> = Key, Value, Opts, State) ->
%%     {ok, Key, Value, State};
%% attribute(_Tag, <<"style">>, Value, Opts, State) ->
%%     {ok, <<"style">>, Value, State};
%% attribute(_Tag, <<"on", _/binary>> = Key, Value, Opts, State) ->
%%     {ok, Key, Value, State};
%% attribute(_Tag, <<"hx-vals">> = Key, Value, Opts, State) ->
%%     {ok, Key, Value, State};
%% attribute(_Tag, <<"hx-", _binary/binary>> = Key, Value, Opts, State) ->
%%     {ok, Key, Value, State};
%% attribute(_Tag, <<"htmx-", _binary/binary>> = Key, Value, Opts, State) ->
%%     {ok, Key, Value, State};
attribute(_Tag, Key, {}, _Opts, State)
  when is_binary(Key) ->
    {ok, Key, <<>>, State};
attribute(_Tag, Key, Value, _Opts, State)
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
content(Content, _Opts, State)
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
bracket_empty(Element, Attributes, Opts, State) ->
    case  attributes(Element, Attributes, Opts, State) of
        {ok, <<>>, _NewState} ->
            <<"<", Element/binary, ">">>;
        {ok, NewAttributes, _NewState} ->
            <<"<", Element/binary, " ", NewAttributes/binary,">">>;
        Elsewise ->
            Elsewise
    end.

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

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
check_file(Filename, #{ root := Root } = _Opts) ->
    case filelib:safe_relative_path(Filename, Root) of
        unsafe ->
            {error, unsafe};
        Elsewise ->
            {ok, Elsewise}
    end.

full_path(Filename, Root) ->
    case filename:pathtype(Filename) of
        absolute -> 
            filename:append(Root, Filename);
        relative ->
            filename:join(Root, Filename)
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
include_raw(Filename, #{ root := Root } = Opts) ->
    FullPath = full_path(Filename, Root),
    case check_file(Filename, Opts) of
        {error, unsafe} ->
            {error, unsafe};
        _ ->
            file:read_file(FullPath)
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
include_template(Filename, #{ root := Root } = Opts) ->
    FullPath = full_path(Filename, Root),
    case check_file(Filename, Opts) of
        {error, unsafe} ->
            {error, unsafe};
        {ok, _} ->
            file:consult(FullPath)
    end.
