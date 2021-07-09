:- module(colors_t, []).

:- use_module(library(plunit)).
:- use_module(tailwind/colors).

:- begin_tests(colors).

test(colors_1,
     [ true( Color == rgb(0x4d, 0x7c, 0x0f)) ]) :-
    phrase(color(Color), `lime-700`).
test(colors_2,
     [ true( Color == rgba(0x44, 0x40, 0x3c, 12)) ]) :-
    phrase(color(Color), `warmGray-700-5`).
test(colors_3,
    [ true( Color == rgb(0xc0, 0xff, 0xee) )]) :-
    phrase(color(Color), `#c0ffee`).
test(colors_4,
    [ true( Color == rgb(0xc0, 0xff, 0xee) )]) :-
    phrase(color(Color), `rgb-c0ffee`).
test(colors_5,
    [ true( Color == rgb(0xa8, 0x55, 0xf7) )]) :-
    phrase(color(Color), `purple-500`).
test(colors_6,
    [ true( Color == rgb(0xff, 0xff, 0xff) )]) :-
    phrase(color(Color), `white`).

:- end_tests(colors).
