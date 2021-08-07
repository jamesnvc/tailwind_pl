:- module(watcher, [start_watching_dirs/3,
                    stop_watching_dirs/1]).
/** <module> filesystem watcher to automatically generate CSS

@author James Cash
*/

:- use_module(library(apply_macros)).
:- use_module(library(apply), [maplist/2]).
:- use_module(library(css_write), [write_css/2, css//1]).
:- use_module(library(filesex), [directory_member/3]).
:- use_module(library(inotify), []).
:- use_module(library(yall)).

:- use_module(library(tailwind_generate), [tw_from_file/2]).

:- dynamic watched_file_css/2.

not_hidden_file(Path) :-
    file_base_name(Path, Name),
    \+ ( sub_atom(Name, 0, _, _, '.') ).

%! start_watching_dirs(+Dirs, +OutputFile, -Watcher) is det.
%
%  Start watching the directories =Dirs= for file changes, outputting
%  the combined CSS output as running tailwind_generate:tw_from_file/2 on each
%  changed file, outputting to =OutputFile=. =Watcher= will be unified
%  with an opaque value which can be passed to stop_watching_dirs/1 to
%  stop the watcher running.
start_watching_dirs(Dirs, OutputFile, queues(WatcherQueue, BuilderQueue)) :-
    inotify:inotify_init(Watch, []),
    maplist({Watch}/[Dir]>>(
                inotify:inotify_add_watch(Watch, Dir, [create, delete])
            ), Dirs),
    forall(( member(Dir, Dirs),
             directory_member(Dir, SubDir, [recursive(true),
                                            exclude('.git'),
                                            exclude_directory('.git'),
                                            file_type(directory)])),
           inotify:inotify_add_watch(Watch, SubDir, [all])),
    forall(( member(Dir, Dirs),
             directory_member(Dir, P, [recursive(true), extensions([pl])])),
           ( not_hidden_file(P),
             inotify:inotify_add_watch(Watch, P, [all]),
             tw_from_file(P, Css),
             assertz(watched_file_css(P, Css)) )),
    generate_output(OutputFile),
    message_queue_create(WatcherQueue),
    message_queue_create(BuilderQueue),
    thread_create(build_css_output(BuilderQueue, OutputFile), _, []),
    thread_create(handle_file_changed(BuilderQueue, WatcherQueue, Watch), _, []).

%! stop_watching_dirs(+Watcher) is det.
%
%  Stop the watcher that was started with the opaque watcher =Watcher=.
stop_watching_dirs(queues(WatcherQueue, BuilderQueue)) :-
    thread_send_message(WatcherQueue, done),
    thread_send_message(BuilderQueue, done),
    retractall(watched_file_css(_, _)).

handle_file_changed(BQ, WQ, Watch) :-
    inotify:inotify_read_event(Watch, Event, [timeout(0.5)]), !,
    catch(
        handle_event(Watch, BQ, Event),
        Err,
        debug(tailwind, "Error handling event: ~w", [Err])
    ),
    handle_file_changed(BQ, WQ, Watch).
handle_file_changed(BQ, WQ, Watch) :-
    thread_get_message(WQ, _, [timeout(0.5)])
    ->  inotify:inotify_close(Watch)
    ;  handle_file_changed(BQ, WQ, Watch).

handle_event(Watch, BQ, create(file(File))) :-
    not_hidden_file(File),
    inotify:inotify_add_watch(Watch, File, [all]),
    tw_from_file(File, Css),
    assertz(watched_file_css(File, Css)),
    thread_send_message(BQ, update).
handle_event(Watch, _, create(directory(Dir))) :-
    not_hidden_file(Dir),
    inotify:inotify_add_watch(Watch, Dir, [all]).
handle_event(Watch, BQ, delete(file(File))) :-
    not_hidden_file(File),
    inotify:inotify_rm_watch(Watch, File),
    retractall(watched_file_css(File, _)),
    thread_send_message(BQ, update).
handle_event(_, BQ, modify(file(File))) :-
    tw_from_file(File, NewCss),
    ( watched_file_css(File, OldCss) -> true ; OldCss = [] ),
    ( NewCss == OldCss
    -> debug(tailwind, "no changes", [])
    ; ( transaction((retractall(watched_file_css(File, _)),
                     assertz(watched_file_css(File, NewCss)))),
        thread_send_message(BQ, update) )).
handle_event(W, BQ, close_write(file(F))) :-
    handle_event(W, BQ, modify(file(F))).
handle_event(_, _, Event) :-
    debug(yyy, "other event ~w", [Event]).

build_css_output(Queue, Outfile) :-
    thread_get_message(Queue, Msg),
    ( Msg == done
    -> true
    ;  ( debug(tailwind, "Updating CSS", []),
         generate_output(Outfile),
         build_css_output(Queue, Outfile) )).

generate_output(Outfile) :-
    setof(Style,
          F^Styles^(
              watched_file_css(F, Styles),
              member(Style, Styles) ),
          Css), !,
    write_css(css(Css), Txt),
    setup_call_cleanup(
        open(Outfile, write, Stream),
        format(Stream, "~s", [Txt]),
        close(Stream)
    ).
generate_output(_) :-
    debug(tailwind, "no css found", []).
