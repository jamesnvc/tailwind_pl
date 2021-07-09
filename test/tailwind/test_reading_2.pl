:- module(test_reading, []).

:- use_module(library(apply_macros)).
:- use_module(library(debug), [debug/3, debug/1]).

thread_pool:create_pool(handler_pool) :-
    thread_pool_create(handler_pool, 20, []).

config_file(F) :- getenv('CONFIG_FILE', F), !.
config_file('../config.pl').

:- setting(port, integer, 8080, 'Port to run API server on').

%! main is det.
%  Non-interactive entry point for the server
main :-
    config_file(Conf),
    load_settings(Conf),
    setting(port, Port),
    go(Port),
    set_cleanup_handlers,
    syslog(notice, "Server started", []),
    thread_get_message(_). % wait

set_cleanup_handlers :-
    on_signal(term, _, shutdown),
    on_signal(int, _, shutdown),
    on_signal(usr2, _, reload_config).

shutdown(_) :-
    setting(port, Port),
    stop(Port),
    halt.

reload_config(_) :-
    syslog(debug, "Reloading config", []),
    config_file(Conf),
    load_settings(Conf).

%! go(+Port) is det.
%  Interactive entry point to start the server.
go(Port) :-
    debug(server), debug(notify),
    setting(use_syslog, UseSyslog),
    setting(log_topic, LogTopic),
    ( UseSyslog -> openlog(LogTopic, [], syslog) ; true ),
    set_prolog_flag(message_context, [thread, time('%Y-%m-%d %T')]),
    config_file(Conf),
    load_settings(Conf),
    http_server(http_dispatch, [port(Port)]),
    maybe_start_toplevel.

stop(Port) :-
    debug(server, "Stopping...", []),
    http_stop_server(Port, []),
    closelog.

maybe_start_toplevel :-
    setting(remote_toplevel_port, Port),
    Port \= 0, !,
    debug(server, "Starting remote toplevel on port ~w", [Port]),
    prolog_server(Port, [allow(ip(127, 0, 0, 1))]).
maybe_start_toplevel.

% Routes

:- multifile http:location/3.
:- dynamic http:location/3.

http:location(api, root(api), []).

:- http_handler('/', page_handler(home_page(_{})), [method(get), id(home_page),
                                               spawn(handler_pool)]).
:- http_handler('/info', page_handler(info_page), [method(get), id(info_page),
                                                   spawn(handler_pool)]).
:- http_handler('/do/thing', form_handler(greeting),
                [method(post), id(greeting_form), spawn(handler_pool)]).
:- http_handler('/counter/inc', form_handler(counter_inc), [method(post),
                                                            id(counter_inc_form),
                                                            spawn(handler_pool)]).

% Handlers

page_handler(PageDcg, _Request) :-
    reply_html_page(title('Hotwire.pl'), [\PageDcg]).

form_handler(Action, Request) :-
    http_read_data(Request, Data, [form_data(form)]),
    reply_html_page(title('Hotwire.pl'), [\form(Action, Data)]).

% content

user:body(default, Body) -->
    html(
        body([div('data-frame-id'(app), Body), \hotwire_script_tag])).

hotwire_script_tag -->
    js_script({|javascript(_)||
        const parser = new DOMParser();

        const handleResponse = (target, resp) => {
          const doc = parser.parseFromString(resp, "text/html");
          const id = target.getAttribute("data-frame-id");
          const elt = doc.querySelector(`*[data-frame-id="${id}"]`);
          if (target) { target.replaceWith(elt); }
        };

        const findTargetElement = (start_elt) => {
          let elt = start_elt;
          if (elt.getAttribute("data-target")) {
            const target_id = elt.getAttribute("data-target")
            const target = document.querySelector(`*[data-frame-id="${target_id}"]`);
            if (target) { return target; }
          }
          while (elt && !elt.getAttribute("data-frame-id")) {
              elt = elt.parentNode;
          }
          return elt;
        };

        document.body.addEventListener("submit", (evt) => {
          evt.preventDefault();
          evt.stopImmediatePropagation();
          const form = evt.target;
          const target = findTargetElement(form);
          const data = new FormData(form);
          fetch(form.action,
                {method: 'POST',
                 // explicitly not setting content-type
                 // so browser sets boundary properly
                 credentials: 'same-origin',
                 body: data})
          .then((resp) => resp.text())
          .then((resp) => {
              handleResponse(target, resp);
              form.reset();
          })
          .catch(err => {
              console.error("Error submitting form", err);
          })
        });

        const find_link = (element) => {
        let elt = element;
        while (elt &&
                ( elt.nodeType !== Node.ELEMENT_NODE ||
                !elt.getAttribute("href") )) {
            elt = elt.parentNode;
        }
        return elt;
        };

        document.body.addEventListener('click', e => {
          const any_key = e.metaKey || e.altKey || e.ctrlKey || e.shiftKey;
          const link_elt = find_link(e.target);
          const href = link_elt && link_elt.href;
          if (!link_elt || !href) { return; }
          const url = new URL(href);
          const target_elt = link_elt.getAttribute("target") === "_top" ?
                             document.body : findTargetElement(link_elt);
          if (!any_key &&
              ( link_elt.target === "" ||
                  link_elt.target === "_self" ) &&
              e.button === 0 &&
              window.location.hostname === url.hostname &&
              ( !url.port || window.location.port === url.port )) {
            e.preventDefault();
            const loc = window.location;
            const cur_href = loc.pathname + loc.search + loc.hash;
            const rel_href = url.pathname + url.search + url.hash;
            if (cur_href !== rel_href) {
              history.pushState({}, '', rel_href);
            }
            fetch(rel_href,
                  {method: 'GET',
                   credentials: 'same-origin'})
            .then(resp => resp.text())
            .then(resp => handleResponse(target_elt, resp))
            .catch(err => {
              console.error("Error fetching page", err);
            })
          }
        });

        window.onpopstate = (event) => {
            fetch(document.location.pathname,
                {method: 'GET',
                credentials: 'same-origin',
                headers: {'x-hotwire-partial': 1}})
            .then(resp => resp.text())
            .then(resp => handleResponse(resp))
            .catch(err => {
              console.error("Error fetching page", err);
            })
        };
    |}).

% Pages

home_page(Info) -->
    { ( get_dict(name, Info, Name) -> true ; Name = ""),
      ( get_dict(counter, Info, Counter) -> true ; Counter = 0 ) },
    html(
        [
         div('data-frame-id'(main),
             [\app_nav(home_page),
              div('data-frame-id'(greeting),
                  [h1("Hello ~w"-[Name]),
                   form([method(post), action(#(greeting_form))],
                        [input([type(text), name(name), placeholder(name)], []),
                         input([type(text), name(beep), placeholder(thing)], []),
                         button(hello)])
                  ]),

              div('data-frame-id'(counter),
                  [span(Counter),
                   form([action(#(counter_inc_form)), method(post)],
                        [input([type(hidden), value(Counter), name(counter)], []),
                         button("Increment")])
                  ])
             ])
        ]
    ).

info_page -->
    html(
        div('data-frame-id'(main),
            [\app_nav(info_page),
             h1("Info"),
             p(class('hover:animate-bounce'),
               "here's some stuff, some information"),
             a(href(/), "Back to home")
            ])
    ).

app_nav(Active) -->
    html(
        nav([class("bg-red-50 md:mt-1")],
            \nav_links(Active, [home_page-"Main Page", info_page-"Info"]))).

nav_links(_, []) --> [].
nav_links(Active, [Link-Title|Links]) -->
    {  Active == Link -> Cls = active ; Cls = '' },
    html(a([href(#(Link)), class([Cls, 'text-red-500', 'hover:bg-pink-100'])], Title)),
    nav_links(Active, Links).

% handlers

form(greeting, Body) -->
     { memberchk(name=Name, Body),
       Info = _{name: Name, counter: 0} % get counter from elsewhere?
     },
     home_page(Info).

form(counter_inc, Body) -->
    { memberchk(counter=ValS, Body),
      atom_number(ValS, Val),
      NewVal is Val + 1,
      Info = _{name: "", counter: NewVal} % get name from elsewhere?
    },
    home_page(Info).
