%%%===================================================================
%%% @doc draft: Create html page using tuple, map and binaries.
%%%
%%% ```
%%% % create a simple page
%%% Title = {h1, <<"this is my title">>}.
%%% Paragraph1 = {p, <<"long time ago...">>}.
%%% Paragraph2 = {p, <<"that's all folks!">>}.
%%% Paragraph3 = fun(_Opts) -> {ok, {p, <<"end.">>}} end.
%%% Body = [Title, Paragraph1, Paragraph2, Paragraph3].
%%% erml_html:create({html, Body}).
%%% '''
%%%
%%% Will generate this page:
%%%
%%% ```
%%% <<"<html><h1>this is my title</h1>",
%%%   "<p>long time ago...</p>",
%%%   "<p>that&apos;s all folks!</p>",
%%%   "<p>end.</p>",
%%%   "</html>">>
%%% '''
%%%
%%% @todo convert this code as behavior.
%%% @todo when reading a template, compile it as module.
%%% @todo when a template includes dynamic code, allow rendering it
%%%        with custom option in the mode.
%%%
%%% @end
%%%===================================================================
-module(erml_html).
-export([open/1, open/2]).
-export([create/1, create/2]).
-export([table/2]).
-export([join/1, join/2]).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-type tag() :: atom() | list() | binary().
-type attributes() :: #{}.
-type content() :: [].
-type options() :: #{}.
-type element() :: {tag(), attributes()}
                 | {tag(), attributes(), content()}
                 | {tag(), attributes(), content(), options()}
                 | {apply, {atom(), list()}}
                 | {apply, {atom(), atom(), list()}}                 
                 | {include, list() | binary()}
                 | binary().
-type elements() :: [element()].


%%--------------------------------------------------------------------
%% @doc A demo function to generate table.
%% @end
%%--------------------------------------------------------------------
table([], _) ->
    create({table, []});
table([Header|Rest], #{ header := true } = Opts) ->
    Keys = maps:keys(Header),
    Head = table_header(Keys, Header, Opts),
    Body = table_rows(Rest, Keys, [], Opts),
    {table, [Head, Body]};
table([Header|Rest] = Rows, Opts) ->
    Keys = maps:keys(Header),
    Body = table_rows(Rows, Keys, [], Opts),
    {table, Body}.

table_header(Keys, Data, #{ extra := true }) ->
    Th = [ {th, [{span, #{}, Key}
                , <<" ">>
                ,{span, #{}, [<<"(">>, maps:get(Key, Data), <<")">>]}
                ]
           }
           || Key <- Keys ],
    {thead, {tr, #{}, Th}};
table_header(Keys, _, Opts) ->
    {thead, {tr, #{}, [ {th, #{}, Key} || Key <- Keys ]}}.

table_rows([], _, Buffer, _) -> {tbody, Buffer};
table_rows([Last], Keys, Buffer, #{ footer := true }) ->
    Footer = {tfooter, {tr, [ {td, maps:get(Last, Key)} || Key <- Keys ]}},
    Body = {tbody, lists:reverse(Buffer)},
    [Body, Footer];
table_rows([Row|Rest], Keys, Buffer, Opts) ->
    table_rows(Rest, Keys, [{tr, [ {td, [maps:get(Key, Row)]} || Key <- Keys ]}|Buffer], Opts).

%%--------------------------------------------------------------------
%% @doc create a new HTML page from elements.
%% @end
%%--------------------------------------------------------------------
-spec create(Element) -> Return when
      Element :: element() | elements(),
      Return  :: binary().

create(Element) ->
    create(Element, #{}).

%%--------------------------------------------------------------------
%% @doc create a new HTML page from elements.
%% @end
%%--------------------------------------------------------------------
-spec create(Element, Opts) -> Return when
      Element :: element() | elements(),
      Opts    :: options(),
      Return  :: binary().

create(Element, Opts)  ->
    Full = tags(Element, Opts),
    doctype(Full, Opts).

%%--------------------------------------------------------------------
%% @doc create a new HTML page from elements.
%% @end
%%--------------------------------------------------------------------
open(Path) ->
    open(Path, #{}).

open(Path, Opts) ->
    {ok, Data} = file:consult(Path),
    create(Data, Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tags([], _) -> <<>>;
tags(Elements, Opts) ->
    tags(Elements, <<>>, Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tags([], Buffer, _Opts) -> Buffer;
tags(Element, Buffer, Opts)
  when is_tuple(Element) ->
    tags([Element], Buffer, Opts);
tags([Element|Elements], Buffer, Opts)
  when is_tuple(Element) ->
    Tag = tag(Element, Opts),
    tags(Elements, <<Buffer/binary, Tag/binary>>, Opts);
tags([Element|Elements], Buffer, Opts)
  when is_list(Element) ->
    Tags = tags(Element, Opts),
    tags(Elements, <<Buffer/binary, Tags/binary>>, Opts);
tags([Element|Elements], Buffer, Opts) ->
    Encoded = text(Element, Opts),
    tags(Elements, <<Buffer/binary, Encoded/binary>>, Opts);
tags(Element, Buffer, Opts) ->
    Encoded = text(Element, Opts),
    <<Buffer/binary, Encoded/binary>>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
doctype(Buffer, #{ doctype := true }) ->
    <<"<!DOCTYPE html>", Buffer/binary>>;
doctype(Buffer, _Opts) ->
    Buffer.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
text(Content, Opts)
  when is_integer(Content) ->
    text(integer_to_binary(Content), Opts);
text(Content, Opts)
  when is_atom(Content) ->
    text(atom_to_binary(Content), Opts);
text(Content, #{ html_entities := false })
  when is_binary(Content) ->
    Content;
text(Content, _Opts)
  when is_binary(Content) ->
    erml_html_entities:encode(Content).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tag({include_raw, Path}, Opts) ->
    case file:read_file(Path) of
        {ok, Content} ->
            erml_html_entities:encode(Content);
        Elsewise ->
            throw(Elsewise)
    end;
tag({include_template, Path}, Opts) ->
    case file:consult(Path) of
        {ok, Content} ->
            tags([Content], Opts);
        Elsewise ->
            throw(Elsewise)
    end;
tag({include_template, Path, Variables}, Opts) ->
    case file:consult(Path) of
        {ok, Content} ->
            tags([Content], #{ variables => Variables });
        Elsewise ->
            throw(Elsewise)
    end;
% variable support
tag({Variable}, Opts)  ->
    Result = get_variable(Variable, Opts),
    tags([Result], Opts);    
tag({apply, {Function, Args}}, #{module := Module} = Opts)
  when is_atom(Function), is_list(Args) ->
    case apply(Module, Function, [Opts, Args]) of
        {ok, Result} when is_binary(Result) ->
            erml_html_entities:encode(Result);
        {ok, Result} when is_list(Result) ->
            tags(Result, Opts);
        {ok, Result} when is_tuple(Result) ->
            tag(Result, Opts)
    end;
tag({apply, {Module, Function, Args}}, Opts) 
  when is_atom(Module), is_atom(Function), is_list(Args) ->
    case apply(Module, Function, [Opts|Args]) of
        {ok, Result} when is_binary(Result) ->
            erml_html_entities:encode(Result);
        {ok, Result} when is_list(Result) ->
            tags(Result, Opts);
        {ok, Result} when is_tuple(Result) ->
            tag(Result, Opts)
    end;
tag({apply, Fun}, Opts)
  when is_function(Fun, 1) ->
    case Fun(Opts) of
        {ok, Result} when is_binary(Result) ->
            erml_html_entities:encode(Result);
        {ok, Result} when is_list(Result) ->
            tags(Result, Opts);
        {ok, Result} when is_tuple(Result) ->
            tag(Result, Opts)
    end;
tag({Element, Content}, Opts)
  when is_binary(Content) ->
    tag({Element, #{}, Content}, Opts);
% {html, []} should use default or empty attributes. The second
% element of the tuple is the inner content.
tag({Element, Content}, Opts)
  when is_list(Content) ->
    tag({Element, #{}, Content}, Opts);
tag({Element, Content}, Opts)
  when is_tuple(Content) ->
    tag({Element, #{}, Content}, Opts);
% {html, #{}} is a tag without content and using customer attributes.
tag({Element, Attributes}, Opts)
  when is_map(Attributes) ->
    tag({Element, Attributes, []}, Opts);
tag({Element, Attributes, _Content} = Tag, Opts) ->
    tag1(Tag, <<>>, Opts);
tag(Element, Opts) when is_function(Element) ->
    tag1(Element, <<>>, Opts).

tag_test() ->
    % tag can be defined with a triplet
    [?assertEqual(<<"<html></html>">>
                 ,tag({html, #{}, []}, []))

     % tag can be defined with a pair
    ,?assertEqual(<<"<html></html>">>
                 ,tag({html, []}, []))

     % tag can be defined with a pair
    ,?assertEqual(<<"<html>test</html>">>
                 ,tag({html, [<<"test">>]}, []))

     % tag can be defined with a pair
    ,?assertEqual(<<"<html>test</html>">>
                 ,tag({html, <<"test">>}, []))

     % tag's content can be a list containing binary
    ,?assertEqual(<<"<html>test</html>">>
                 ,tag({html, #{}, [<<"test">>]}, []))

     % tag's content can be a binary
    ,?assertEqual(<<"<html>test</html>">>
                 ,tag({html, #{}, <<"test">>}, []))

     % attribute's value can be an atom
    ,?assertEqual(<<"<html id=\"test\">test</html>">>
                 ,tag({html, #{ id => test }, [<<"test">>]}, []))

     % attribute's value can be a binary
    ,?assertEqual(<<"<html id=\"test\">test</html>">>
                 ,tag({html, #{ id => <<"test">> }, [<<"test">>]}, []))

     % attribute's value can be a string
    ,?assertEqual(<<"<html id=\"test\">test</html>">>
                 ,tag({html, #{ id => "test" }, [<<"test">>]}, []))

     % attribute's key can be a binary
    ,?assertEqual(<<"<html id=\"test\">test</html>">>
                 ,tag({html, #{ <<"id">> => "test" }, [<<"test">>]}, []))

     % attributes values must be encoded using html entities
    ,?assertEqual(<<"<html id=\"&amp;test\">test</html>">>
                 ,tag({html, #{ <<"id">> => "&test" }, [<<"test">>]}, []))

     % empty element support
    ,?assertEqual(<<"<html id=\"test\" />">>
                 ,tag({{empty, html}, #{ id => "test"}}, []))
    ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
% support for empty tag
tag1({{empty, Element}, Attributes, Content}, Buffer, Opts)
  when is_atom(Element) ->
    NewElement = atom_to_binary(Element),
    tag1({{empty, NewElement}, Attributes, Content}, Buffer, Opts);
tag1({{empty, Element}, Attributes, Content}, Buffer, Opts)
  when is_binary(Element), is_map(Attributes) ->
    tag_empty({Element, Attributes, Content, #{}}, Buffer, Opts);
% support for non-empty tag
tag1({Element, Attributes, Content}, Buffer, Opts)
  when is_atom(Element) ->
    NewElement = atom_to_binary(Element),
    tag1({NewElement, Attributes, Content}, Buffer, Opts);
tag1({Element, Attributes, Content}, Buffer, Opts)
  when is_binary(Element), is_map(Attributes)  ->
    Item = {Element, Attributes, Content, #{}},
    % @todo add suppot for pre and code tags.
    case Element of        
        <<"base">> -> tag_without_content(Item, Buffer, Opts);
        <<"br">> -> tag_without_content(Item, Buffer, Opts);
        <<"img">> -> tag_without_content(Item, Buffer, Opts);
        <<"input">> -> tag_without_content(Item, Buffer, Opts);
        <<"link">> -> tag_without_content(Item, Buffer, Opts);
        <<"meta">> -> tag_without_content(Item, Buffer, Opts);
        <<"source">> -> tag_without_content(Item, Buffer, Opts);
        _ -> tag_with_content(Item, Buffer, Opts)
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tag_with_content({Element, Attributes, Content, _LocalOpts}, _Buffer, Opts) ->
    NewAttributes = attributes(Attributes, Opts),
    StartElement = bracket(Element, NewAttributes),
    NewContent = tags(Content, Opts),
    EndElement = bracket_end(Element),
    <<StartElement/binary, NewContent/binary, EndElement/binary>>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tag_without_content({Element, Attributes, _Content, _LocalOpts}, _Buffer, Opts) ->
    NewAttributes = attributes(Attributes, Opts),
    Item = bracket(Element, NewAttributes),
    <<Item/binary>>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tag_empty({Element, Attributes, _Content, _LocalOpts}, _Buffer, Opts) ->
    NewAttributes = attributes(Attributes, Opts),
    Item = bracket_empty(Element, NewAttributes),
    <<Item/binary>>.

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

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
attributes(Attributes, Opts)
  when map_size(Attributes) =:= 0 ->
    <<>>;
attributes(Attributes, Opts) ->
    Keys = maps:keys(Attributes),
    attributes(Attributes, Keys, [], Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
attributes(_Attributes, [], Buffer, Opts) ->
    join(lists:reverse(Buffer), <<" ">>);
attributes(Attributes, [Key|Keys], Buffer, Opts) ->
    Value = erlang:map_get(Key, Attributes),
    Attribute = attribute(Key, Value, Opts),
    attributes(Attributes, Keys, [Attribute|Buffer], Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @todo cleanup the mess for htmx
%% @end
%%--------------------------------------------------------------------
attribute(Key, Value, Opts) ->
    case key(Key, Opts) of
        % css
        {ok, <<"style", _binary>> = NewKey} ->
            NewValue = value(Value, Opts),
            Quoted = single_quote(NewValue),
            <<NewKey/binary,"=", Quoted/binary>>;
        % this is a javascript like attribute
        {ok, <<"on", _binary>> = NewKey} ->
            NewValue = value(Value, Opts),
            Quoted = single_quote(NewValue),
            <<NewKey/binary,"=", Quoted/binary>>;
        % this is an htmx attribute, must be a valid json.
        {ok, <<"hx-vals">> = NewKey} ->
            NewValue = value(Value, Opts),
            Quoted = single_quote(NewValue),
            <<NewKey/binary,"=", Quoted/binary>>;
        % standard htmx attributes
        {ok, <<"hx-", _/binary>> = NewKey} ->
            NewValue = value(Value, Opts),
            Quoted = double_quote(NewValue),
            <<NewKey/binary,"=", Quoted/binary>>;
        % this is an htmx attribute
        {ok, <<"htmx-", _/binary>> = NewKey} ->
            NewValue = value(Value, Opts),
            Quoted = double_quote(NewValue),
            <<NewKey/binary,"=", Quoted/binary>>;
        {ok, <<"href">> = NewKey} ->
            NewValue = value(Value, Opts),
            Quoted = double_quote(NewValue),
            <<NewKey/binary,"=", Quoted/binary>>;
        % javascript element
        {ok, <<"src">> = NewKey} ->
            NewValue = value(Value, Opts),
            Quoted = single_quote(NewValue),
            <<NewKey/binary,"=", Quoted/binary>>;
        % that's another kind of key
        {ok, NewKey} ->
            NewValue = value(Value, Opts),
            Encoded = erml_html_entities:encode(NewValue),
            Quoted = double_quote(Encoded),
            <<NewKey/binary, "=", Quoted/binary>>
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
join(Binaries) -> join(Binaries, <<" ">>).

join(Binaries, Sep) -> join(Binaries, Sep, <<>>).
join([Binary], _, Buffer) ->
    <<Binary/binary, Buffer/binary>>;
join([Binary|Rest], Sep, Buffer) ->
    join(Rest, Sep, <<Sep/binary, Binary/binary, Buffer/binary>>).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
get_variable(Variable, #{variables := Variables} = Opts) ->
    case maps:get(Variable, Variables, '$undefined') of
        '$undefined' -> 
            throw({error, {undefined, Variable}});
        Content -> Content
    end;
get_variable(Variable, Opts) ->
    throw({error, {unset, variables}}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
value({Variable}, Opts) ->
    get_variable(Variable, Opts);
value(Value, _Opts) when is_list(Value) ->
    list_to_binary(Value);
value(Value, _Opts) when is_atom(Value) ->
    atom_to_binary(Value);
value(Value, _Opts) when is_integer(Value) ->
    integer_to_binary(Value);
value(Value, _Opts) when is_binary(Value) ->
    Value.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
key(Key, _Opts) when is_list(Key) ->
    {ok, list_to_binary(Key)};
key(Key, _Opts) when is_atom(Key) ->
    {ok, atom_to_binary(Key)};
key(Key, _Opts) when is_integer(Key) ->
    {ok, integer_to_binary(Key)};
key(Key, _Opts) when is_binary(Key) ->
    {ok, Key};
key(Key, Opts) ->
    {error, Key}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
double_quote(Value) ->
    <<$\", Value/binary, $\">>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
single_quote(Value) ->
    <<$\', Value/binary, $\'>>.
