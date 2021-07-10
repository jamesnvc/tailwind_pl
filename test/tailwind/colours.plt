:- module(colours_t, []).

:- use_module(library(plunit)).
:- use_module(tailwind/colours).

:- begin_tests(colours).

test(colours_1,
     [ true( Colour == rgb(0x4d, 0x7c, 0x0f)) ]) :-
    phrase(colour(Colour), `lime-700`).
test(colours_2,
     [ true( Colour == rgba(0x44, 0x40, 0x3c, 12)) ]) :-
    phrase(colour(Colour), `warmGray-700-5`).
test(colours_3,
    [ true( Colour == rgb(0xc0, 0xff, 0xee) )]) :-
    phrase(colour(Colour), `#c0ffee`).
test(colours_4,
    [ true( Colour == rgb(0xc0, 0xff, 0xee) )]) :-
    phrase(colour(Colour), `rgb-c0ffee`).
test(colours_5,
    [ true( Colour == rgb(0xa8, 0x55, 0xf7) )]) :-
    phrase(colour(Colour), `purple-500`).
test(colours_6,
    [ true( Colour == rgb(0xff, 0xff, 0xff) )]) :-
    phrase(colour(Colour), `white`).

:- end_tests(colours).
