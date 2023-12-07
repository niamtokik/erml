# Extending Erml

## Implementing Open Graph protocol

```erlang
-module(opengraph).
-export([]).


template(Name, Content) ->
  {meta, #{ property => Name, content => Content}}.

title(Content) ->
  template("og:title", Content).
  
type(Content) ->
  template("og:type", Content).
  
url(Content) ->
  template("og:url", Content);

image(Content) ->
  template("og:image", Content).

audio(Content) ->
  template("og:audio", Content).
  
description(Content) ->
  template("og:description", Content).
  
determiner(Content) ->
  template("og:determiner", Content).

locale(Content) ->
  template("og:locale", Content).

local_alternate(Content) ->
  template("og:locale:alternate", Content).
  
site_name(Content) ->
  template("og:site_name", Content).
  
video(Content) ->
  template("og:video", Content).
```

## Implementing Twitter Card

## References and Resources

  - https://ogp.me/
  - https://schema.org/
  - https://www.dublincore.org/specifications/dublin-core/dcmi-terms/
  - https://www.digitalocean.com/community/tutorials/how-to-add-twitter-card-and-open-graph-social-metadata-to-your-webpage-with-html
  - http://html5doctor.com/microdata/
