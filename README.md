# Tailwind_pl

Tailwind_pl is a pack for SWI-Prolog that lets you do CSS styling with [TailwindCSS](https://tailwindcss.com/docs) without any Node.js dependency. It is effectively a rewrite of [Tailwind's JIT compiler](https://tailwindcss.com/docs/just-in-time-mode) in Prolog, inspired by [Girouette](https://github.com/green-coder/girouette)'s approach in Clojure.

Tailwind_pl watches the given directories for changes to Prolog files, searches for Tailwind-style CSS selectors and generates a CSS file containing just the used selectors.

Tailwind_pl also extends Tailwind by allowing more dynamic class-names that use exact colours or numbers (ex. `w-42%`).


## Installation

``` prolog
?- pack_install(tailwind_pl).
```

## Example Usage


``` prolog
  :- module(myapp, [go/1]).

:- use_module(library(filesex), [directory_file_path/3, make_directory_path/1]).
:- use_module(library(http/http_dispatch), [http_redirect/3,
                                            http_dispatch/1,
                                            http_reply_file/3,
                                            http_handler/3,
                                            http_location_by_id/2
                                           ]).
:- use_module(library(http/html_write), [html//1,
                                         reply_html_page/2,
                                         html_receive//1]).
:- use_module(library(http/thread_httpd), [http_server/2,
                                           http_stop_server/2]).
:- use_module(library(tailwind), [start_watching_dirs/3,
                                  stop_watching_dirs/1,
                                  reset_style/1]).

%! go(+Port) is det.
%  Interactive entry point to start the server.
go(Port) :-
    start_css_watcher,
    http_server(http_dispatch, [port(Port)]).

user:file_search_path(css, CssDir) :-
    working_directory(Cwd, Cwd),
    directory_file_path(Cwd, "../resources/css", CssDir).

:- dynamic css_watcher/1.
start_css_watcher :-
    stop_css_watcher,
    working_directory(Cwd, Cwd),
    directory_file_path(Cwd, "../resources/css", CssDir),
    ( exists_directory(CssDir) -> true ; make_directory_path(CssDir) ),
    absolute_file_name(css("styles.css"), CssFile, [access(none)]),
    start_watching_dirs([Cwd], CssFile, Watcher),
    assertz(css_watcher(Watcher)).

stop_css_watcher :-
    css_watcher(Watcher), !,
    stop_watching_dirs(Watcher),
    retractall(css_watcher(Watcher)).
stop_css_watcher.

% Routes

:- multifile http:location/3.
:- dynamic http:location/3.

http:location(css, root(css), []).

:- http_handler(root(.), home_page_handler, [method(get), id(home_page),
                                             spawn(handler_pool)]).

:- http_handler(css('styles.css'), css_file_handler, [method(get), id(tw_css)]).
:- http_handler(css('reset.css'), reset_css_handler, [method(get), id(reset_css)]).

home_page_handler(_Request) :-
    reply_html_page(title('Hotwire.pl'), [\home_page]).

css_file_handler(Request) :-
    http_reply_file(css('styles.css'), [], Request).

reset_css_handler(_) :-
    reset_style(Style),
    format("Content-Type: text/css~n~n~s", [Style]).

% content

user:head(default, Head) -->
    html([Head,
          \html_receive(head),
          link([rel(stylesheet), href(#(reset_css))], []),
          link([rel(stylesheet), href(#(tw_css))], [])
         ]).

% Home Page

home_page -->
    html(
        div(class(["flex-col", "text-purple-500", 'bg-rose-50']),
            [h2(class(['text-lg',
                       'hover:animation-ping',
                       'bg-green-300', 'hover:text-rose-100']),
                "Info"),
             \p("here's some stuff, some information"),
             \p("Another paragraph")
            ])
    ).

p(Text) -->
    html(p(class(['text-sm', 'text-blue', 'hover:opacity-50']),
           Text)).
```

## Differences from Tailwind

All the selectors from Tailwind should also be supported here.
If any don't work, please file a bug!

Additionally, some new features are supported, taking advantage of the fact that the styles are created dynamically as needed.

  - Colours can be specified not just as the predefined colour names (e.g. `pink-500`) but as RGB(A) and HSL(A); for example, all of the following are valid styles: `text-#c0ffee`, `bg-rgb-cafeba`, `from-hsla-50-60-70-0.5`. This feature is copied from Girouette.

  - in most places where a size is given as just a number in Tailwind (e.g. `mx-4` which will set the left and right margins to `1rem`), you can use arbitrary distances -- e.g. `mx-2.3rem, `font-size-17.5px`. This feature is copied from Girouette.

  - Similar to the `group-hover:`, `group-focus:`, etc, you can use `group-attr-SOMEATTR:` which will effective do `.group[SOMEATTR]`, as `group-hover` does `.group:hover`. This can be used, for example, to do put the `group` class on a `<details>` element and `group-attr-open:hidden` on some child, do hide said element when the detail element is open
