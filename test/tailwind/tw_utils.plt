:- module(tw_utils_t, []).

:- use_module(library(plunit)).
:- use_module(tailwind/tw_utils).

:- begin_tests(tw_utils).


test(length_1,
     [ true(Len == length(11.7, vh)) ]) :-
    phrase(length(Len), `11.7vh`).

test(value_unit_1,
     [ true(Css == "3rem") ]) :-
    value_unit_css(
        number(12), Css,
        _{number: _{unit: rem,
                    value_fn: div_4}}).

test(one_of_1,
    [ true(Found == bar), nondet ]) :-
    phrase(one_of([`foo`, `bar`], Found), `bar`).

test(alternates_1,
     [ true(Found == length(17.5, rem)) ]) :-
    phrase(alternates([percentage(Found),
                       length(Found),
                       num(Found)]),
           `17.5rem`).


:- end_tests(tw_utils).
