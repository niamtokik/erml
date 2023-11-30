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
    [tag].

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
%%
%%--------------------------------------------------------------------
simple_generator() ->
    {ok, <<"test">>}.
