%%%===================================================================
%%%
%%%===================================================================
-module(erml_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [tag, variables, include_raw, include_template
    ,custom_tag, attributes].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
tag() ->
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
tag(_Config) ->
    % empty document
    {ok, <<>>} = erml_html5:compile([]),
    {ok, <<>>} = erml_html5:compile(<<>>),

    % simple tag without attributes
    {ok, <<"<html></html>">>} = erml_html5:compile({html, []}),
    {ok, <<"<html></html>">>} = erml_html5:compile({<<"html">>, []}),
    {ok, <<"<html></html>">>} = erml_html5:compile({"html", []}),
    {ok, <<"<html><body></body></html>">>}
        = erml_html5:compile({html, {body, []}}),

    % simple tag with attributes
    {ok, <<"<html></html>">>} = erml_html5:compile({html, #{}, []}),
    {ok, <<"<html></html>">>} = erml_html5:compile({<<"html">>, #{}, []}),
    {ok, <<"<html></html>">>} = erml_html5:compile({"html", #{}, []}),
    {ok, <<"<html><body></body></html>">>}
        = erml_html5:compile({html, #{}, {body, #{}, []}}),

    % simple tag with attributes support.
    {ok, <<"<html><body class=\"test\"></body></html>">>}
        = erml_html5:compile({html, #{}, {body, #{class => "test"}, []}}),
    {ok, <<"<html><body class=\"test\"></body></html>">>}
        = erml_html5:compile({html, #{}, {body, #{class => test}, []}}),
    {ok, <<"<html><body class=\"test\"></body></html>">>}
        = erml_html5:compile({html, #{}, {body, #{class => <<"test">>}, []}}),

    % content and entities support
    {ok, <<"<html><body>test</body></html>">>}
        = erml_html5:compile({html, {body, <<"test">>}}),
    {ok, <<"<html><body>test</body></html>">>}
        = erml_html5:compile({html, {body, test}}),
    {ok, <<"<html><body>123</body></html>">>}
        = erml_html5:compile({html, {body, 123}}),
    {ok, <<"<html><body>1.00000000000000000000e&plus;00</body></html>">>}
        = erml_html5:compile({html, {body, 1.0}}),
    {ok, <<"<html><body><p>that&apos;s a test!</p></body></html>">>}
        = erml_html5:compile([{html, {body, {p, [<<"that's a test!">>]}}}]),

    % dynamic template (gen_server call by default)
    {ok, [ <<"<html><body>">>
         , {gen_server,call,server,paragraph,1000}
         , <<"</body></html>">>
         ]
    } = erml_html5:compile({html, {body, {call, server, paragraph}}}),

    % dynamic template (gen_server call)
    {ok, [ <<"<html><body>">>
         , {gen_server,call,server,paragraph,1000}
         , <<"</body></html>">>
         ]
    } = erml_html5:compile({html, {body, {gen_server, call, server, paragraph}}}),

    % dynamic template (gen_statem call)
    {ok, [ <<"<html><body>">>
         , {gen_statem,call,server,paragraph,1000}
         , <<"</body></html>">>
         ]
    } = erml_html5:compile({html, {body, {gen_statem, call, server, paragraph}}}),

    % special tags
    % {ok, <<"<pre></pre>">>} = erml_html5:compile({pre, []}),
    % {ok, <<"<pre>test\ndata</pre>">>}
    %    = erml_html5:compile({pre, ["test", "data"]}),
    % {ok, <<"<pre><code>test\ndata</code></pre>">>}
    %     = erml_html5:compile({pre, {code, ["test", "data"]}}),

    % explicit content with some features
    {ok, <<"test">>}
        = erml_html5:compile({content, "test"}),
    {ok, <<"test&amp;">>}
        = erml_html5:compile({content, "test&"}),
    {ok, <<"test&apos;">>}
        = erml_html5:compile({content, <<"test'">>}),
    {ok,<<"test">>}
        = erml_html5:compile({content, test}),
    {ok,<<"test&">>}
        = erml_html5:compile({content, <<"test&">>, #{entities => false}}),

    % more complex case
    {ok, <<"<html><head><title>test</title></head>"
           "<body><h1>title</h1><p>paragraph</p></body>"
           "</html>">>}
        = erml_html5:compile({html, [{head, {title, test}}
                                 ,{body, [{h1, title}
                                         ,{p, paragraph}
                                         ]}
                                 ]}),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
custom_tag() -> [].
custom_tag(_Config) ->
    % custom void element
    {ok, <<"<html>">>} = erml_html5:compile({{empty, html}, []}),

    % void elements support: area, base, br, col, embed, hr, img,
    % input, link, meta, source, track, wbr
    {ok, <<"<input>">>} = erml_html5:compile({input, #{}}),
    {ok, <<"<input>">>} = erml_html5:compile({<<"input">>, #{}}),
    {ok, <<"<input>">>} = erml_html5:compile({"input", #{}}),

    {ok, <<"<area>">>} = erml_html5:compile({area, #{}}),
    {ok, <<"<area>">>} = erml_html5:compile({<<"area">>, #{}}),
    {ok, <<"<area>">>} = erml_html5:compile({"area", #{}}),

    {ok, <<"<base>">>} = erml_html5:compile({base, #{}}),
    {ok, <<"<base>">>} = erml_html5:compile({"base", #{}}),
    {ok, <<"<base>">>} = erml_html5:compile({<<"base">>, #{}}),

    {ok, <<"<br>">>} = erml_html5:compile({br, #{}}),
    {ok, <<"<br>">>} = erml_html5:compile({"br", #{}}),
    {ok, <<"<br>">>} = erml_html5:compile({<<"br">>, #{}}),

    {ok, <<"<col>">>} = erml_html5:compile({col, #{}}),
    {ok, <<"<col>">>} = erml_html5:compile({<<"col">>, #{}}),
    {ok, <<"<col>">>} = erml_html5:compile({"col", #{}}),

    {ok, <<"<embed>">>} = erml_html5:compile({embed, #{}}),
    {ok, <<"<embed>">>} = erml_html5:compile({<<"embed">>, #{}}),
    {ok, <<"<embed>">>} = erml_html5:compile({"embed", #{}}),

    {ok, <<"<hr>">>} = erml_html5:compile({hr, #{}}),
    {ok, <<"<hr>">>} = erml_html5:compile({"hr", #{}}),
    {ok, <<"<hr>">>} = erml_html5:compile({<<"hr">>, #{}}),

    {ok, <<"<img>">>} = erml_html5:compile({img, #{}}),
    {ok, <<"<img>">>} = erml_html5:compile({"img", #{}}),
    {ok, <<"<img>">>} = erml_html5:compile({<<"img">>, #{}}),

    {ok, <<"<link>">>} = erml_html5:compile({link, #{}}),
    {ok, <<"<link>">>} = erml_html5:compile({<<"link">>, #{}}),
    {ok, <<"<link>">>} = erml_html5:compile({"link", #{}}),

    {ok, <<"<meta>">>} = erml_html5:compile({meta, #{}}),
    {ok, <<"<meta>">>} = erml_html5:compile({<<"meta">>, #{}}),
    {ok, <<"<meta>">>} = erml_html5:compile({"meta", #{}}),

    {ok, <<"<source>">>} = erml_html5:compile({source, #{}}),
    {ok, <<"<source>">>} = erml_html5:compile({<<"source">>, #{}}),
    {ok, <<"<source>">>} = erml_html5:compile({"source", #{}}),

    % attributes
    {ok, <<"<img src=\"/test.png\">">>} = erml_html5:compile({img, #{ src => "/test.png" }}),
    ok.    

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
variables() -> [].
variables(_Config) ->
    % serializer stop when a variable is present and variables key is
    % not set
    {stop,{not_configured,variables,#{}},#{}}
        = erml_html5:compile({v}, #{}),

    % if the variable is not found, the serializer stop
    {stop,{not_found,v},#{}}
        = erml_html5:compile({v}, #{variables => #{test=>1}}),

    % simple variable usages
    {ok, <<"1">>} = erml_html5:compile({v}, #{variables => #{v=>1}}),
    {ok, <<"test">>} = erml_html5:compile({v}, #{variables => #{v=><<"test">>}}),
    {ok, <<"test">>} = erml_html5:compile({v}, #{variables => #{v=>test}}),

    % a variable can be of any type you want.
    {ok, <<"test">>} = erml_html5:compile([{1}], #{variables => #{1 => <<"test">>}}),
    {ok, <<"test">>} = erml_html5:compile([{"1"}], #{variables => #{"1" => <<"test">>}}),
    {ok, <<"test">>} = erml_html5:compile([{<<"1">>}], #{variables => #{<<"1">> => <<"test">>}}),
    {ok, <<"test">>} = erml_html5:compile([{#{}}], #{variables => #{#{} => <<"test">>}}),

    % multivariable support
    {ok,<<"123">>}
        = erml_html5:compile([{v1},{v2},{v3}], #{variables => #{v1=>1, v2=>2, v3=>3}}),

    % yes, we can include a variable into another variable, loop
    % protection exist thought...
    {ok, <<"aba">>}
        = erml_html5:compile([{v1},{v2},{v3}], #{variables => #{v1=>a, v2=> <<"b">>, v3=>{v1}}}),

    % ... and we can have an example here.
    % {stop,{variable_recursion,v3},_}
    %    = erml_html5:compile([{v1},{v2},{v3}], #{variables => #{v1=>a, v2=> <<"b">>, v3=>{v3}}}),

    % we can also create tags from variable.
    {ok,<<"<p>test</p>">>}
        = erml_html5:compile([{tag}], #{variables => #{tag => {p, [<<"test">>]}}}),

    % and so a full page.
    Html = {html, [{head}, {body}]},
    Head = {head, []},
    Body = {body, [{title}, {paragraph}]},
    {ok,<<"<html><head></head>"
          "<body><h1>title</h1><p>paragraph</p></body>"
          "</html>">>}
        = erml_html5:compile(Html, #{variables => #{ head => Head
                                                , body => Body
                                                , title => {h1, title}
                                                , paragraph => {p, paragraph}}}),
    ok.

%%--------------------------------------------------------------------
%% @doc @todo module/function calls can be added directly in any erml
%% template.
%%
%% ```
%% {apply, Module, Function, Arguments}.
%% {apply, Module, Function, Arguments, Opts}.
%% '''
%%
%% @end
%%--------------------------------------------------------------------
function_call() -> [].
function_call(_Config) ->
    {ok, <<"test">>}
        = erml:create({apply, erml_SUITE, echo, [<<"test">>]}),

    {ok, <<"test">>}
        = erml:create({apply, erml_SUITE, echo, [<<"test">>], #{}}),

    ok.

%%--------------------------------------------------------------------
%% @doc @todo lambda (private function) call can be only added during code
%% execution.
%%
%% ```
%% {apply, Fun}.
%% {apply, Fun, Opts}.
%% '''
%%
%% @end
%%--------------------------------------------------------------------
lambda_call() -> [].
lambda_call(_Config) ->
    Fun_001 = fun() -> {ok, <<"test">>} end,
    {ok, <<"test">>} = erml:create({apply, Fun_001}),

    Fun_002 = fun(_Opts) -> {ok, <<"test">>} end,
    {ok, <<"test">>} = erml:create({apply, Fun_001}),

    Fun_003 = fun(_Opts) -> {ok, <<"test">>} end,
    {ok, <<"test">>} = erml:create({apply, Fun_001, #{}}),
    ok.

%%--------------------------------------------------------------------
%% @doc @todo raw inclusion includes a raw file without any conversion.
%%
%% ```
%% {include_raw, Path}.
%% {include_raw, Path, Opts}.
%% '''
%%
%% @end
%%--------------------------------------------------------------------
include_raw() -> [].
include_raw(Config) ->
    DataDir = ?config(data_dir,Config),
    Opts = #{ root => DataDir },
    {ok, <<"Lorem Ipsum\n\n", _/binary>>} = erml_html5:compile({include_raw, "raw_ascii.txt"}, Opts),
    {stop, _, _} = erml_html5:compile({include_raw, "../raw.txt"}, Opts),
    {stop, _, _} = erml_html5:compile({include_raw, "../../raw.txt"}, Opts),
    {stop, _, _} = erml_html5:compile({include_raw, "./test/../../raw.txt"}, Opts),
    ok.

%%--------------------------------------------------------------------
%% @doc
%%
%% ```
%% {include_template, Path}.
%% {include_template, Path, Opts}.
%% '''
%%
%% @end
%%--------------------------------------------------------------------
include_template() -> [].
include_template(Config) ->
    DataDir = ?config(data_dir,Config),
    Opts = #{ root => DataDir },
    {ok, <<"<html><head><title>Lorem Ipsum</title></head><body>", _/binary>>} 
        = erml_html5:compile({include, "page_simple.erml"}, Opts),
    {ok, <<"<ul><li>In order to handle failure", _/binary>>} 
        = erml_html5:compile({include, "list_simple.erml"}, Opts),
    {stop, _, _} = erml_html5:compile({include, "../page_simple.erml"}, Opts),
    {stop, _, _} = erml_html5:compile({include, "./test../page_simple.erml"}, Opts),
    ok.

%%--------------------------------------------------------------------
%% @doc Media inclusion is an important feature to help developers to
%% deal with multimedia content without having to deal with complex
%% code.
%%
%% ```
%% {include_media, Path}.
%% {include_media, Path, Opts}.
%% {include_media, Path, Opts, Attributes}.
%% '''
%% @end
%%--------------------------------------------------------------------
include_media() -> [].
include_media(_Config) ->
    % A special include form dealing with media.
    % an HTML page is directly added without transformation
    {ok, <<"<p>data</p>">>}
        = erml_html5:compile({include_media, "test.html", #{}}),

    % an image is converted as base64 and inserted in an <img> tag
    {ok, <<"<img src=\"...\">">>}
        = erml_html5:compile({include_media, "test.jpg", #{}}),

    % an audio file will produce an audio tag
    {ok, <<"<audio controls src=\"...\"></audio>">>}
        = erml_html5:compile({include_media, "test.mp3", #{}}),

    % a video file should also act in the same way
    {ok, <<"<video controls><source src=\"...\"></video>">>}
        = erml_html5:compile({include_media, "test.mp4", #{}}),

    % An enforced type can be supplied to be sure included data is the
    % correct one used.
    {ok, <<"<audio controls src=\"...\"></audio>">>}
        = erml_html5:compile({include_media, {audio, "test.mp4"}, #{}}),

    % an external url can be used to include external media. A cache
    % system should probably be used and a support for selecting
    % different kind of client could also be great.
    {ok, <<"<img src=\"...\"">>}
        = erml_html5:compile({include_media, "https://localhost:8080", #{}}),

    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
include_callback() -> [].
include_callback(_Config) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
attributes() -> [].
attributes(_Config) ->
    % @TODO key must only contain printable characters
    % @TODO value must be converted to html entities or if it contains
    %       a double quote, use single quote, if it uses single quote,
    %       use double quotes.

    % void attribute
    {ok, <<"<p test></p>">>}
        = erml_html5:compile({p, #{ test => {} }, []}),

    % simple attribute
    {ok, <<"<p id=\"test\"></p>">>}
        = erml_html5:compile({p, #{ id => <<"test">>}, []}),
    {ok, <<"<p id=\"test\"></p>">>}
        = erml_html5:compile({p, #{ id => "test"}, []}),
    {ok, <<"<p id=\"test\"></p>">>}
        = erml_html5:compile({p, #{ id => test}, []}),

    % multi attributes
    {ok, <<"<p id=\"test\" class=\"test\"></p>">>}
        = erml_html5:compile({p, #{ id => "test", class => "test"}, []}),
    {ok, <<"<p id=\"test\" class=\"test\"></p>">>}
        = erml_html5:compile({p, #{ id => "test", class => <<"test">>}, []}),
    {ok, <<"<p id=\"test\" class=\"test\"></p>">>}
        = erml_html5:compile({p, #{ id => "test", class => test}, []}),
    
    % dynamic attribute using fun/0
    % FunRandomValue = fun() -> {ok, <<"random">>} end,
    % {ok, <<"<p id=\"random\"></p>">>}
    %     = erml_html5:compile({p, #{ id => FunRandomValue }, []}),

    % dynamic attribute using fun/1
    % FunRandomValue = fun(_Opts) -> {ok, <<"random">>} end,
    % {ok, <<"<p id=\"random\"></p>">>}
    %     = erml_html5:compile({p, #{ id => FunRandomValue }, []}),

    % attribute value can also be defined in variables.
    % {ok, <<"input id=\"test\">">>}
    %     = erml_html5:compile({input, #{ id => {id} }, []}, #{ variables => #{ id => test }}),

    % attributes can be generated with a fun/0
    % FunRandomAttributes = fun() -> {ok, #{ id => random}} end,
    % {ok, <<"<p id=\"random\"></p>">>}
    %     = erml_html5:compile({p, FunRandomAttributes, []}),

    % attributes can be generated with a fun/1
    % FunRandomAttributes = fun(_Opts) -> {ok, #{ id => random}} end,
    % {ok, <<"<p id=\"random\"></p>">>}
    %     = erml_html5:compile({p, FunRandomAttributes, []}),

    % attributes can also be used from variable,
    % {ok, <<"<p id=\"test\"></p>">>}
    %     = erml_html5:compile({p, {attributes}, []}, #{ variables => #{ attributes => #{ id => test } }}),

    % here an example with a CSRF Token protection
    % Csrf = fun() -> {ok, #{ type => hidden
    %                       , name => "CSRFToken"
    %                       , value => <<"8Ioo1T3SPaqi+33B91/leiysWYbGeqTN4zZqIfzu5us=">> }}
    %        end,
    % {ok, <<"<input type=\"hidden\" "
    %        "name=\"CSRFToken\" " 
    %        "value=\"8Ioo1T3SPaqi+33B91/leiysWYbGeqTN4zZqIfzu5us=\"">>}
    %     = erml_html5:compile({input, Csrf(), []}),

    % void attribute
    {ok, <<"<p test></p>">>}
        = erml_html5:compile({p, #{ test => {} }, []}),

    % list support
    {ok, <<"<p test=\"1\"></p>">>}
        = erml_html5:compile({p, [{test, 1}], []}),
    {ok, <<"<p test></p>">>}
        = erml_html5:compile({p, [test], []}),

    % Fun = fun() -> {ok, <<"random">>} end,
    % {ok, <<"<p id=\"data\" class=\"random\" test></p>">>}
    %     = erml_html5:compile({p, [test, {id, data}, {random, Fun}], []}),

    ok.    

    
