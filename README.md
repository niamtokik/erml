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
