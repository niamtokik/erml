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

### `{Tag, Content}`


