%%%===================================================================
%%% @doc
%%% see https://picturepan2.github.io/spectre
%%% @end
%%%===================================================================
-module(erml_spectre).
-compile([export_all]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
requirement() ->
    requirement(unpkg).

requirement(locale) ->
    [{link, #{ rel => "stylesheet", href => "/style/spectre.min.css" }}
    ,{link, #{ rel => "stylesheet", href => "/style/spectre-exp.min.css" }}
    ,{link, #{ rel => "stylesheet", href => "/style/spectre-icons.min.css" }}
    ];    
requirement(unpkg) ->
    [{link, #{ rel => "stylesheet", href => "https://unpkg.com/spectre.css/dist/spectre.min.css" }}
    ,{link, #{ rel => "stylesheet", href => "https://unpkg.com/spectre.css/dist/spectre-exp.min.css" }}
    ,{link, #{ rel => "stylesheet", href => "https://unpkg.com/spectre.css/dist/spectre-icons.min.css" }}
    ].

%%--------------------------------------------------------------------
%% @doc exported callback.
%% @end
%%--------------------------------------------------------------------
element({button, Name}, _Opts, State) ->
    {erml, button(Name), State};
element({form, Content}, _Opts, State) ->
    {erml, form(Content), State};
element({input_text, Name, Attributes}, _Opts, State) ->
    {erml, input_text(Name, Attributes), State};
element({container, Content}, _Opts, State) ->
    {erml, container(Content), State}.
    
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
button(Name) ->
    button(Name, #{ class => "btn" }).

button(Name, Attributes) ->
    Merged = maps:merge(#{ class => "btn"}, Attributes),
    {button, Merged, Name}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
form(Content) ->
    {form, #{ class => "form-group"}, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
input_text(Name, Attributes) ->
    {'div', #{ class => "form-group"}, [
      {label, #{}, Name},
      {input, #{ class => "form-input", type => "text"}, []}
    ]}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
container(Content) ->
    {'div', #{ class => "container" }, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
hero(Content) ->
    {'div', #{class => "hero hero-lg"}, [
      {'div', #{class => "hero-body"}, Content}
    ]}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
loading() ->
    {'div', #{class => "loading"}, []}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
card(Content) ->
    {'div', #{class => "card"}, Content}.

card_header(Content) ->
    {'div', #{class => "card-header"}, Content}.

card_footer(Content) ->
    {'div', #{class => "card-footer"}, Content}.

card_body(Content) ->
    {'div', #{class => "card-body"}, Content}.

card_image(Content) ->
    {'div', #{class => "card-image"}, Content}.

card_title(Content) ->
    {'div', #{class => "card-title"}, Content}.

card_subtitle(Content) ->
    {'div', #{class => "card-subtitle"}, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
menu(Content) ->
    {'ul', #{class => "menu"}, Content}.

menu_divider(Content) ->
    {'li', #{class => "divider", <<"data-contents">> => Content}, []}.

menu_item(Content) ->
    {'li', #{class => "menu-item"}, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
dropdown(Content) ->
    {'div', #{class => "dropdown"}, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
chip_span(Content) ->
    {'span', #{class => "chip"}, Content}.

chip_div(Content) ->
    {'div', #{class => "chip"}, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tile(Content) ->
    {'div', #{class => "tile"}, Content}.

tile_icon(Content) ->
    {'div', #{class => "tile-icon"}, Content}.

tile_content(Content) ->
    {'div', #{class => "tile-content"}, Content}.

tile_title(Content) ->
    {p, #{class => "tile-title"}, Content}.

tile_subtitle(Content) ->
    {p, #{class => "tile-subtitle"}, Content}.

tile_action(Content) ->
    {'div', #{class => "tile-action"}, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
breadcrumb(Content) ->
    {'ul', #{class => "breadcrumb"}, Content}.

breadcrumb_item(Content) ->
    {'li', #{class => "breadcrumb-item"}, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
label(Content) ->
    {'span', #{class => "label"}, Content}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-define(ICON(NAME), icon(NAME) -> {'i', #{class => "icon icon-" ++ NAME}, []}).
               
?ICON("arrow-up");
?ICON("arrow-right");
?ICON("arrow-left");
?ICON("arrow-down");
?ICON("upward");
?ICON("forward");
?ICON("downward");
?ICON("back");
?ICON("caret");
?ICON("menu");
?ICON("apps");
?ICON("more-horiz");
?ICON("more-vert").

