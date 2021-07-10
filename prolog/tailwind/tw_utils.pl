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
/** <module> Tailwind Utils

Collection of utility predicates and DCGS for parsing tailwind.

@author James Cash
*/

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

%! value_unit_css(+Value, -CssValue, +Options) is det.
%
%  Convert a parsed value =Value= into a CSS value, using the options
%  =Options= to control the units applied to bare numbers as well as
%  scaling factors.
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

%! one_of(+EltsList, -MatchAtom)// is semidet.
%
%  Finds the first element in =EltsList= that matches and unifies
%  =MatchAtom= with the matching element as an atom.
one_of([Elt|_], EltAtom) -->
    Elt,
    { atom_codes(EltAtom, Elt) }.
one_of([_|Elts], Match) --> one_of(Elts, Match).

%! alternates(+DcgsList)// is semidet.
%
%  Meta-DCG that finds the first DCG in =DcgsList= that successfully
%  matches.
alternates([Dcg|_]) --> Dcg, !.
alternates([_|Dcgs]) --> alternates(Dcgs).

%! signus(-Sign)// is semidet.
%
%  DCG to parse a "sign" value -- + or -
signus(S) --> one_of(["+", "-"], S).
%! direction(-Dir)// is semidet.
%
%  DCG to parse a "direction" value -- top, right, bottom, or left.
direction(direction(D)) --> one_of(["t", "r", "b", "l"], D).
%! axis(-Axis)// is semidet.
%
%  DCG to parse an axis value -- x or y.
axis(axis(A)) --> one_of(["x", "y"], A).

%! auto(-Auto)// is semidet.
%
%  DCG to parse an "auto" value.
auto(auto) --> "auto".
%! full_100(-Full)// is semidet.
%
%  DCG to parse a "full" value
full_100(full_100) --> "full".
%! screen_100vh(-Height)// is semidet.
%
%  DCG to parse a "full-screen" height value.
screen_100vh(screen_100vh) --> "screen".
%! screen_100vw(-Width)// is semidet.
%
%  DCG to parse a "full-screen" width value.
screen_100vw(screen_100vw) --> "screen".
%! min_content(-Min)// is semidet.
%
%  DCG to parse a min-content value.
min_content(min_content) --> "min".
%! max_content(-Min)// is semidet.
%
%  DCG to parse a max-content value.
max_content(max_content) --> "max".

%! num(-Num)// is semidet.
%
%  DCG to parse a number.
num(number(N)) --> number(N).

%! fraction(-Num)// is semidet.
%
%  DCG to parse a fraction.
fraction(fraction(N)) -->
    integer(Num), "/", integer(Denom),
    { N is Num / Denom }.

%! percentage(-Percentage)// is semidet.
%
%  DCG to parse a percentage value.
percentage(percentage(P)) --> number(P), "%".

%! length(-Length)// is semidet.
%
%  DCG to parse a CSS length value.
length(length(L, Unit)) -->
    number(L), length_unit(unit(Unit)).

%! length_unit(-Unit)// is semidet.
%
%  DCG to parse a CSS length unit.
length_unit(unit(Unit)) -->
    one_of(["cm", "mm", "in", "pc", "pt", "px",
            "em", "ex", "ch", "rem", "lh", "vw", "vh", "vmin", "vmax"],
           Unit), !.

%! angle(-Angle)// is semidet.
%
%  DCG to parse a CSS angle value.
angle(angle(A, Unit)) -->
    number(A), one_of(["deg", "grad", "rad", "turn"], Unit).

%! time(-Time)// is semidet.
%
%  DCG to parse a CSS time value.
time(time(T, Unit)) --> number(T), one_of(["s", "ms"], Unit).

%! resolution(-Res)// is semidet.
%
%  DCG to parse a CSS resolution value.
resolution(res(R, Unit)) -->
    number(R), one_of(["dpi", "dpcm", "dppx", "x"], Unit).
