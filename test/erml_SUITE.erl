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
    [tag, variables].

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
    {ok, <<>>} = erml_tag:create([]),
    {ok, <<>>} = erml_tag:create(<<>>),
    
    % simple tag without attributes
    {ok, <<"<html></html>">>} = erml_tag:create({html, []}),
    {ok, <<"<html></html>">>} = erml_tag:create({<<"html">>, []}),
    {ok, <<"<html></html>">>} = erml_tag:create({"html", []}),
    {ok, <<"<html><body></body></html>">>} 
        = erml_tag:create({html, {body, []}}),

    % simple tag with attributes
    {ok, <<"<html></html>">>} = erml_tag:create({html, #{}, []}),
    {ok, <<"<html></html>">>} = erml_tag:create({<<"html">>, #{}, []}),
    {ok, <<"<html></html>">>} = erml_tag:create({"html", #{}, []}),
    {ok, <<"<html><body></body></html>">>} 
        = erml_tag:create({html, #{}, {body, #{}, []}}),

    % simple tag with attributes support.
    {ok, <<"<html><body class=\"test\"></body></html>">>}
        = erml_tag:create({html, #{}, {body, #{class => "test"}, []}}),
    {ok, <<"<html><body class=\"test\"></body></html>">>}
        = erml_tag:create({html, #{}, {body, #{class => test}, []}}),
    {ok, <<"<html><body class=\"test\"></body></html>">>}
        = erml_tag:create({html, #{}, {body, #{class => <<"test">>}, []}}),

    % content and entities support
    {ok, <<"<html><body>test</body></html>">>}
        = erml_tag:create({html, {body, <<"test">>}}),
    {ok, <<"<html><body>test</body></html>">>}
        = erml_tag:create({html, {body, test}}),
    {ok, <<"<html><body>123</body></html>">>}
        = erml_tag:create({html, {body, 123}}),
    {ok, <<"<html><body>1.00000000000000000000e&plus;00</body></html>">>}
        = erml_tag:create({html, {body, 1.0}}),
    {ok, <<"<html><body><p>that&apos;s a test!</p></body></html>">>}
        = erml_tag:create([{html, {body, {p, [<<"that's a test!">>]}}}]),

    % dynamic template (gen_server call by default)
    {ok, [ <<"<html><body>">>
         , {gen_server,call,server,paragraph,1000}
         , <<"</body></html>">> 
         ]
    } = erml_tag:create({html, {body, {call, server, paragraph}}}),

    % dynamic template (gen_server call)
    {ok, [ <<"<html><body>">>
         , {gen_server,call,server,paragraph,1000}
         , <<"</body></html>">>
         ]
    } = erml_tag:create({html, {body, {gen_server, call, server, paragraph}}}),

    % dynamic template (gen_statem call)
    {ok, [ <<"<html><body>">>
         , {gen_statem,call,server,paragraph,1000}
         , <<"</body></html>">>
         ]
    } = erml_tag:create({html, {body, {gen_statem, call, server, paragraph}}}),

    % special tags
    {ok, <<"<pre></pre>">>} = erml_tag:create({pre, []}),
    {ok, <<"<pre>test\ndata</pre>">>} 
        = erml_tag:create({pre, ["test", "data"]}),
    {ok, <<"<pre><code>test\ndata</code></pre>">>}
        = erml_tag:create({pre, {code, ["test", "data"]}}),

    % explicit content with some features
    {ok, <<"test">>}
        = erml_tag:create({content, "test"}),
    {ok, <<"test&amp;">>}
        = erml_tag:create({content, "test&"}),
    {ok, <<"test&apos;">>} 
        = erml_tag:create({content, <<"test'">>}),
    {ok,<<"test">>}
        = erml_tag:create({content, test}),
    {ok,<<"test&">>}
        = erml_tag:create({content, <<"test&">>, #{entities => false}}),

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
        = erml_tag:create({v}, #{}),

    % if the variable is not found, the serializer stop
    {stop,{not_found,v},#{}}
        = erml_tag:create({v}, #{variables => #{test=>1}}),

    % simple variable usages
    {ok, <<"1">>} = erml_tag:create({v}, #{variables => #{v=>1}}),
    {ok, <<"test">>} = erml_tag:create({v}, #{variables => #{v=><<"test">>}}),
    {ok, <<"test">>} = erml_tag:create({v}, #{variables => #{v=>test}}),

    % a variable can be of any type you want.
    {ok, <<"test">>} = erml_tag:create([{1}], #{variables => #{1 => <<"test">>}}),
    {ok, <<"test">>} = erml_tag:create([{"1"}], #{variables => #{"1" => <<"test">>}}),
    {ok, <<"test">>} =  erml_tag:create([{<<"1">>}], #{variables => #{<<"1">> => <<"test">>}}),
    {ok, <<"test">>} = erml_tag:create([{#{}}], #{variables => #{#{} => <<"test">>}}),

    % multivariable support
    {ok,<<"123">>} 
        = erml_tag:create([{v1},{v2},{v3}], #{variables => #{v1=>1, v2=>2, v3=>3}}),

    % yes, we can include a variable into another variable, loop
    % protection exist thought...
    {ok, <<"aba">>}
        = erml_tag:create([{v1},{v2},{v3}], #{variables => #{v1=>a, v2=> <<"b">>, v3=>{v1}}}),

    % ... and we can have an example here.
    {stop,{variable_recursion,v3},_} 
        = erml_tag:create([{v1},{v2},{v3}], #{variables => #{v1=>a, v2=> <<"b">>, v3=>{v3}}}),

    % we can also create tags from variable.
    {ok,<<"<p>test</p>">>}
        = erml_tag:create([{tag}], #{variables => #{tag => {p, [<<"test">>]}}}),
        
    % and so a full page.
    Html = {html, [{head}, {body}]},
    Head = {head, []},
    Body = {body, [{title}, {paragraph}]},
    {ok,<<"<html><head><body><h1>title<p>paragraph</p></h1></body></head></html>">>}
        = erml_tag:create(Html, #{variables => #{ head => Head
                                                , body => Body
                                                , title => {h1, title}
                                                , paragraph => {p, paragraph}}}),
    ok.

%%--------------------------------------------------------------------
%% @doc module/function calls can be added directly in any erml
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
%% @doc lambda (private function) call can be only added during code
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
%% @doc raw inclusion includes a raw file without any conversion. 
%%
%% ```
%% {include_raw, Path}.
%% {include_raw, Path, Opts}.
%% '''
%%
%% @end
%%--------------------------------------------------------------------
include_raw() -> [].
include_raw(_Config) ->
    {ok, <<"test">>} = erml:create({include_raw, "raw.txt"}),
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
include_template(_Config) ->
    {ok, <<"<p>test</p>">>} 
        = erml:create({include_template, "paragraph.erml"}),
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
        = erml_tag:create({include_media, "test.html", #{}}),

    % an image is converted as base64 and inserted in an <img> tag 
    {ok, <<"<img src=\"...\">">>} 
        = erml_tag:create({include_media, "test.jpg", #{}}),

    % an audio file will produce an audio tag
    {ok, <<"<audio controls src=\"...\"></audio>">>}
        = erml_tag:create({include_media, "test.mp3", #{}}),

    % a video file should also act in the same way
    {ok, <<"<video controls><source src=\"...\"></video>">>}
        = erml_tag:create({include_media, "test.mp4", #{}}),
    
    % An enforced type can be supplied to be sure included data is the
    % correct one used.
    {ok, <<"<audio controls src=\"...\"></audio>">>}
        = erml_tag:create({include_media, {audio, "test.mp4"}, #{}}),

    % an external url can be used to include external media. A cache
    % system should probably be used and a support for selecting
    % different kind of client could also be great.
    {ok, <<"<img src=\"...\"">>}
        = erml_tag:create({include_media, "https://localhost:8080", #{}}),
    
    ok.
