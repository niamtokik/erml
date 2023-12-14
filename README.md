# erml

An html library for Erlang, and an sandbox for Erlang, Cowboy, Htmx
Spectre, Mnesia (ECHSM).

## Build

```erlang
rebar3 compile
```

## Usage

```erlang
{html, [
  {head, [
    {title, <<"my website">>},
    {meta, #{ name => "twitter:card", content => "summary"}}
  ]},
  {body, [
    
  ]}
]}.
```

## Documentation

| **Stability** | **Elements** | **Notes** |
|---------------|--------------|-----------|
| |
| | **Standard Elements**
|s| `{Tag, Content}` | Create an html element without attributes
|s| `{Tag, Attributes, Content}` | Create an html element with attributes
|s| `{{empty, Tag}, Attributes}` | Create an empty element
|s| `{content, Content}` | Add raw content
| |
| | **Variables**
|s| `{Variable}` | Insert a variable
| |
| | **Inclusion**
|u| `{include, Path}` | include an erml file
|u| `{include, Path, Opts}` | include an erml file with custom options
|u| `{include_raw, Path}` | include a raw file
|u| `{include_raw, Path, Opts}` | include a raw file with custom options
|u| `{template, Type, Data, Param}` | insert a template
|u| `{template_file, Type, Filename, Param}` | insert a template file
| |
| | **Functions/Modules call**
|s| `{apply, Module, Function, Arguments}` | call a module/function
|s| `{apply, Function, Arguments}` | call a function 
|s| `{apply, Function}` | call a function
| |
| | **Process Call**
|u| `{call, Pid, Message}` | call a gen_server process
|u| `{call, Pid, Message, Timeout}` | call a gen_server process
|u| `{gen_server, call, Pid, Message}` | call a gen_server process
|u| `{gen_server, call, Pid, Message, Timeout}` | call a gen_server process
|u| `{gen_statem, call, Pid, Message}` | call a gen_statem process
|u| `{gen_statem, call, Pid, Message, Timeout}` | call a gen_statem process

Where `u`: unstable; `s`: stable

### Creating Standard Elements

Standard elements are creating using tuples with 2 or 3 elements. A
simple element can be created with an empty list.

```erlang
{html, []}.
```

```html
<html></html>
```

Attributes can be added.

```erlang
{html, #{id => "page"}, []}.
```

```html
<html id="page"></html>
```

Other elements can be added as contents.

```erlang
{html, #{}, 
  {body, []}
}.
```

```html
<html><body></body></html>
```

```erlang
{html, #{}, [
  {head, []}, 
  {body, []}
]}.
```

```html
<html><head></head><body></body></html>
```

It supports also empty elements.

```erlang
{{empty, image}, #{}}.
```

```html
<image>
```

```erlang
{{empty, image}, #{id => test}}
```

```html
<image id=\"test\">
```

```erlang
{{empty, image}, [selected]}.
```

```html
<image selected>
```

Few elements are empty by default, and the content will be
automatically converted as attributes or ignored.

```erlang
{meta, #{name => "twitter:card", content => "summary"}}.
```

```html
<meta name="twitter:card" content="summary">
```

### Using Variables

Variables are created using tuples with one element and defined in
`variables` parameters key.

```erlang
% define variables
Options = #{ 
  variables => #{ 
    test => <<"hello world!">> 
  } 
}.

% create erml template
Erml = {body, [
  {p, {test}}
]}.

% compile it.
erml:compile(Erml, Option).
```

```html
<body><p>hello world!</p></body>
```

Variables can also includes erml data structure.

```erlang
% define variables
Options = #{ 
  variables => #{ 
    test => {span, #{class => "bold"}, <<"hello world!">>}
  } 
}.

% create erml template
Erml = {body, [
  {p, {test}}
]}.

% compile it.
erml:compile(Erml, Option).
```

```html
<body><p><span class=\"bold\">hello world!</span></p></body>
```

### Including Templates and Files

Files and templates can be added from a specific path. By default,
`priv` directory is used as root.

```erlang
{body, [{include_raw, "examples/erlang-punch/lorem_ipsum.txt"}]}.
```

```html
<body>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</body>
```

`erml` files can also be added.

```erlang
{body, {include, "examples/erlang-punch/hero_01.erml"}}.
```

```html
<body><div style="min-height: 100vh;" class="hero bg-primay">...
```

### Calling Functions and Modules

Functions and modules can be called.

```erlang
{body, {apply, fun() -> {ok, <<"hello world!">>} end}}.
```

```html
<body>hello world!</body>
```

### Calling Running Processes

Active processes using `gen_server` or `gen_statem` can be called.
