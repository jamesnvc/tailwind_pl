:- module(generate, [text_tw_css/2,
                     tw_from_file/2]).
/** <module> generate CSS from text containing selectors

Predicates for generating tailwind CSS stylesheets based on used selectors

@author James Cash
*/

:- use_module(library(apply_macros)).
:- use_module(library(apply), [maplist/2, convlist/3]).
:- use_module(library(pcre), [re_replace/4]).
:- use_module(library(ordsets), [list_to_ord_set/2]).
:- use_module(library(yall)).

:- use_module(grammar, [tailwind//1, prefixes//2]).

tw_selector(Media, Variants, Styles) -->
    prefixes(Media, Variants), tailwind(Styles).

word_style(Word, Style) :-
    string_codes(Word, Codes),
    re_replace("([^-a-zA-Z0-9])"/g, "\\$1", Word, EscapedWord),
    format(atom(Sel), ".~w", [EscapedWord]),
    phrase(tw_selector(Media, Variants, Styles), Codes),
    ( is_list(Styles)
    -> partition(['@keyframes'(_, _)]>>true, Styles, Anims, Styles0)
    ;  ( Anims = [], Styles0 = Styles) ),
    make_style(Sel, Media, Variants, Styles0, Style_),
    ( Anims == []
    -> Style = Style_
    ;  Style = [Style_|Anims]).

apply_inner(BaseSel, [], BaseSel) :- !.
apply_inner(BaseSel, Inners, Selector) :-
    maplist([inner(Attr), Attr]>>true, Inners, Attrs),
    atomic_list_concat(Attrs, ':', PseudoClasses),
    format(atom(Selector), "~w:~w", [BaseSel, PseudoClasses]).

wrap_outers(BaseSel, [], BaseSel) :- !.
wrap_outers(BaseSel, Outers, Selector) :-
    maplist([outer(Sel), Sel]>>true, Outers, OuterSels),
    % does having multiple outer variants even make sense?
    atomic_list_concat(OuterSels, '', OuterSel),
    format(atom(Selector), "~w ~w", [OuterSel, BaseSel]).

make_style(ClsName, [], [], &(ChildSel), Style) :- !,
    Style =.. [ClsName, [], ChildSel].
make_style(ClsName, [], [], Styles, Style) :- !,
    Style =.. [ClsName, Styles].
make_style(ClsName, [], Variants, Styles, Css) :- !,
    partition([outer(_)]>>true, Variants, Outers, Inners),
    apply_inner(ClsName, Inners, InnerSelector),
    wrap_outers(InnerSelector, Outers, Selector),
    make_style(Selector, [], [], Styles, Css).
make_style(ClsName, Media, Variants, Styles, '@media'(and(Media), NestedStyle)) :-
    make_style(ClsName, [], Variants, Styles, NestedStyle).

%! text_tw_css(+Text, -Css) is det.
%
%  True when =Text= is an atom or string containing zero or more
%  space-seperated Tailwind-style selectors and =Css= is the
%  corresponding list of CSS styles, suitable to be fed to
%  css_write:write_css/2.
text_tw_css(Text, Css) :-
    text_to_string(Text, String),
    split_string(String, " ", "", Strs),
    convlist(word_style, Strs, Css0),
    merge_keyframe_styles(Css0, Css, []).

merge_keyframe_styles([], Tail, Tail) :- !.
merge_keyframe_styles([C|Cs], Tail0, Tail) :-
    is_list(C), !,
    append(C, Tail1, Tail0),
    merge_keyframe_styles(Cs, Tail1, Tail).
merge_keyframe_styles([C|Cs], [C|Tail0], Tail) :-
    merge_keyframe_styles(Cs, Tail0, Tail).

% like read_file_to_terms, put ignoring quasi_quotes
% can't just pass quasi_quotations(_) to =read_file_to_terms=
% because each call of read_terms/3 tries to unify the variable to different values
read_file_to_terms_noqq(File, Terms) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_stream_to_terms_noqq(Stream, Terms),
        close(Stream)
    ).

read_stream_to_terms_noqq(Stream, Terms) :-
    read_term(Stream, C0, [quasi_quotations(_), syntax_errors(quiet)]),
    read_stream_to_terms_noqq(C0, Stream, Terms).

read_stream_to_terms_noqq(end_of_file, _, []) :- !.
read_stream_to_terms_noqq(C, Stream, [C|Ts]) :-
    read_term(Stream, C2, [quasi_quotations(_), syntax_errors(quiet)]),
    read_stream_to_terms_noqq(C2, Stream, Ts).

text_from_file(File, UniqTexts) :-
    read_file_to_terms_noqq(File, Terms), !,
    extract_text_from_terms(Terms, Texts, []),
    list_to_ord_set(Texts, UniqTexts).
text_from_file(_, []).

extract_text_from_terms([], Tail, Tail1) => Tail = Tail1.
extract_text_from_terms([Term|Terms], Tail, Tail0), string(Term) =>
    Tail = [Term|NewTail],
    extract_text_from_terms(Terms, NewTail, Tail0).
extract_text_from_terms([Term|Terms], Tail, Tail0), atom(Term) =>
    Tail = [Term|NewTail],
    extract_text_from_terms(Terms, NewTail, Tail0).
extract_text_from_terms([Term|Terms], Tail, Tail0), is_list(Term) =>
    extract_text_from_terms(Term, Tail, NewTail),
    extract_text_from_terms(Terms, NewTail, Tail0).
extract_text_from_terms([Term|Terms], Tail, Tail0), compound(Term) =>
    Term =.. [_|Children],
    extract_text_from_terms(Children, Tail, NewTail),
    extract_text_from_terms(Terms, NewTail, Tail0).
extract_text_from_terms([_|Terms], Tail, Tail0) =>
    extract_text_from_terms(Terms, Tail, Tail0).

%! tw_from_file(+File, -Css) is det.
%
%  When =File= is the path to a file, then =Css= will be the
%  corresponding list of CSS selectors & styles corresponding to all
%  the tailwind-style selectors found inside =File=, suitable to be
%  used as input to css_write:write_css/2.
tw_from_file(File, Css) :-
    text_from_file(File, Texts),
    all_tws_from(Texts, Css).

all_tws_from(Text, Css) :-
    all_tws_from(Text, Css-Css, _-[]).

all_tws_from([], List-Tail, List-Tail) :- !.
all_tws_from([Text|Texts], Csses-Tail0, Csses-Tail) :-
    text_tw_css(Text, Css),
    append(Css, Tail1, Tail0),
    all_tws_from(Texts, Csses-Tail1, Csses-Tail).
