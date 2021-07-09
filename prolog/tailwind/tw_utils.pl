:- module(tw_utils, [value_unit_css/3,
                     one_of//2,
                     alternates//1,
                     signus//1,
                     direction//1,
                     axis//1,
                     auto//1,
                     num//1,
                     fraction//1,
                     percentage//1,
                     length//1,
                     length_unit//1,
                     angle//1,
                     time//1,
                     resolution//1
                    ]).

:- use_module(library(dcg/basics), [integer//1, number//1]).
:- use_module(library(dcg/high_order), [sequence//3]).

get_dict_default(K, Dict, V, Default) :-
    ( V = Dict.get(K) -> true ; V = Default ).

div_4(X, Y) :- Y is X / 4.

div_100(X, Y) :- Y is X / 100.

mul_100(X, Y) :- Y is X * 100.

unit_for(percentage(_), '%') :- !.
unit_for(Term, Unit) :-
    functor(Term, _, Ar),
    Ar == 2, !,
    arg(2, Term, Unit).
unit_for(_, "").

value_unit_css(auto, "auto", _):- !.
value_unit_css(none, "none", _):- !.
value_unit_css(full, "full", _):- !.
value_unit_css(min_content, "min-content", _):- !.
value_unit_css(max_content, "max-content", _):- !.
value_unit_css(full_100, "100%", _):- !.
value_unit_css(screen_100vw, "100vw", _):- !.
value_unit_css(screen_100vh, "100vh", _):- !.
value_unit_css(unit(Unit), Css, Opts) :- !,
    value_unit_css(length(1, Unit), Css, Opts).
value_unit_css(Term, Css, Opts) :-
    functor(Term, Type, _),
    unit_for(Term, Unit_),
    ( ValueFn = Opts.get(Type/value_fn)
    -> true
    ;  ValueFn = Opts.get(value_fn)
    -> true
    ;  ValueFn = [X, X]>>true),
    arg(1, Term, N),
    call(ValueFn, N, Value),
    ( Unit = Opts.get(Type/unit)
    -> true
    ;  Unit = Opts.get(unit)
    -> true
    ;  Unit = Unit_ ),
    ( ZeroUnit = Opts.get(Type/zero_unit)
    -> true
    ;  get_dict_default(zero_unit, Opts, ZeroUnit, "") ),
    get_dict_default(signus, Opts, Signus, +),
    number_unit(Value, Unit, ZeroUnit, Signus, Css).

number_unit(0, _, Unit, _, Css) :- !,
    format(string(Css), "0~w", [Unit]).
number_unit(N, Unit, _, Sign, Css) :-
    ( Sign == '-' -> V is N * -1 ; V = N ),
    format(string(Css), "~w~w", [V, Unit]).

one_of([Elt|_], EltAtom) -->
    Elt,
    { atom_codes(EltAtom, Elt) }.
one_of([_|Elts], Match) --> one_of(Elts, Match).

alternates([Dcg|_]) --> Dcg, !.
alternates([_|Dcgs]) --> alternates(Dcgs).

signus(S) --> one_of(["+", "-"], S).
direction(direction(D)) --> one_of(["t", "r", "b", "l"], D).
axis(axis(A)) --> one_of(["x", "y"], A).

auto(auto) --> "auto".
full_100(full_100) --> "full".
screen_100vh(screen_100vh) --> "screen".
screen_100vw(screen_100vw) --> "screen".
min_content(min_content) --> "min".
max_content(max_content) --> "max".

num(number(N)) --> number(N).

fraction(fraction(N)) -->
    integer(Num), "/", integer(Denom),
    { N is Num / Denom }.

percentage(percentage(P)) --> number(P), "%".

length(length(L, Unit)) -->
    number(L), length_unit(unit(Unit)).

length_unit(unit(Unit)) -->
    one_of(["cm", "mm", "in", "pc", "pt", "px",
            "em", "ex", "ch", "rem", "lh", "vw", "vh", "vmin", "vmax"],
           Unit), !.

angle(angle(A, Unit)) -->
    number(A), one_of(["deg", "grad", "rad", "turn"], Unit).

time(time(T, Unit)) --> number(T), one_of(["s", "ms"], Unit).

resolution(res(R, Unit)) -->
    number(R), one_of(["dpi", "dpcm", "dppx", "x"], Unit).
