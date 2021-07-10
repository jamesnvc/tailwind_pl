:- module(colours, [colour//1,
                    has_alpha/1,
                    colour_with_alpha/3,
                    as_transparent/2,
                    colour_css/2]).

:- use_module(library(dcg/basics), [xdigits//1, number//1]).
:- use_module(library(dcg/high_order), [optional//2]).

:- discontiguous colour//1.

% pre-defined colours

term_expansion(predefined_colour(Name, RGB), DcgDef) :-
    string_codes(Name, NameCodes),
    hex_bytes(RGB, [R, G, B]),
    expand_term((predef_colour(rgb(R, G, B)) --> NameCodes, !),
                DcgDef).

predefined_colour("white", "ffffff").
predefined_colour("black", "000000").

predefined_colour("rose-100", "ffe4e6").
predefined_colour("rose-200", "fecdd3").
predefined_colour("rose-300", "fda4af").
predefined_colour("rose-400", "fb7185").
predefined_colour("rose-500", "f43f5e").
predefined_colour("rose-600", "e11d48").
predefined_colour("rose-700", "be123c").
predefined_colour("rose-800", "9f1239").
predefined_colour("rose-900", "881337").
predefined_colour("rose-50", "fff1f2").

predefined_colour("pink-100", "fce7f3").
predefined_colour("pink-200", "fbcfe8").
predefined_colour("pink-300", "f9a8d4").
predefined_colour("pink-400", "f472b6").
predefined_colour("pink-500", "ec4899").
predefined_colour("pink-600", "db2777").
predefined_colour("pink-700", "be185d").
predefined_colour("pink-800", "9d174d").
predefined_colour("pink-900", "831843").
predefined_colour("pink-50", "fdf2f8").

predefined_colour("fuchsia-100", "fae8ff").
predefined_colour("fuchsia-200", "f5d0fe").
predefined_colour("fuchsia-300", "f0abfc").
predefined_colour("fuchsia-400", "e879f9").
predefined_colour("fuchsia-500", "d946ef").
predefined_colour("fuchsia-600", "c026d3").
predefined_colour("fuchsia-700", "a21caf").
predefined_colour("fuchsia-800", "86198f").
predefined_colour("fuchsia-900", "701a75").
predefined_colour("fuchsia-50", "fdf4ff").

predefined_colour("purple-100", "f3e8ff").
predefined_colour("purple-200", "e9d5ff").
predefined_colour("purple-300", "d8b4fe").
predefined_colour("purple-400", "c084fc").
predefined_colour("purple-500", "a855f7").
predefined_colour("purple-600", "9333ea").
predefined_colour("purple-700", "7e22ce").
predefined_colour("purple-800", "6b21a8").
predefined_colour("purple-900", "581c87").
predefined_colour("purple-50", "faf5ff").

predefined_colour("violet-100", "ede9fe").
predefined_colour("violet-200", "ddd6fe").
predefined_colour("violet-300", "c4b5fd").
predefined_colour("violet-400", "a78bfa").
predefined_colour("violet-500", "8b5cf6").
predefined_colour("violet-600", "7c3aed").
predefined_colour("violet-700", "6d28d9").
predefined_colour("violet-800", "5b21b6").
predefined_colour("violet-900", "4c1d95").
predefined_colour("violet-50", "f5f3ff").

predefined_colour("indigo-100", "e0e7ff").
predefined_colour("indigo-200", "c7d2fe").
predefined_colour("indigo-300", "a5b4fc").
predefined_colour("indigo-400", "818cf8").
predefined_colour("indigo-500", "6366f1").
predefined_colour("indigo-600", "4f46e5").
predefined_colour("indigo-700", "4338ca").
predefined_colour("indigo-800", "3730a3").
predefined_colour("indigo-900", "312e81").
predefined_colour("indigo-50", "eef2ff").

predefined_colour("blue-100", "dbeafe").
predefined_colour("blue-200", "bfdbfe").
predefined_colour("blue-300", "93c5fd").
predefined_colour("blue-400", "60a5fa").
predefined_colour("blue-500", "3b82f6").
predefined_colour("blue-600", "2563eb").
predefined_colour("blue-700", "1d4ed8").
predefined_colour("blue-800", "1e40af").
predefined_colour("blue-900", "1e3a8a").
predefined_colour("blue-50", "eff6ff").

predefined_colour("lightBlue-100", "e0f2fe").
predefined_colour("lightBlue-200", "bae6fd").
predefined_colour("lightBlue-300", "7dd3fc").
predefined_colour("lightBlue-400", "38bdf8").
predefined_colour("lightBlue-500", "0ea5e9").
predefined_colour("lightBlue-600", "0284c7").
predefined_colour("lightBlue-700", "0369a1").
predefined_colour("lightBlue-800", "075985").
predefined_colour("lightBlue-900", "0c4a6e").
predefined_colour("lightBlue-50", "f0f9ff").

predefined_colour("cyan-100", "cffafe").
predefined_colour("cyan-200", "a5f3fc").
predefined_colour("cyan-300", "67e8f9").
predefined_colour("cyan-400", "22d3ee").
predefined_colour("cyan-500", "06b6d4").
predefined_colour("cyan-600", "0891b2").
predefined_colour("cyan-700", "0e7490").
predefined_colour("cyan-800", "155e75").
predefined_colour("cyan-900", "164e63").
predefined_colour("cyan-50", "ecfeff").

predefined_colour("teal-100", "ccfbf1").
predefined_colour("teal-200", "99f6e4").
predefined_colour("teal-300", "5eead4").
predefined_colour("teal-400", "2dd4bf").
predefined_colour("teal-500", "14b8a6").
predefined_colour("teal-600", "0d9488").
predefined_colour("teal-700", "0f766e").
predefined_colour("teal-800", "115e59").
predefined_colour("teal-900", "134e4a").
predefined_colour("teal-50", "f0fdfa").

predefined_colour("emerald-100", "d1fae5").
predefined_colour("emerald-200", "a7f3d0").
predefined_colour("emerald-300", "6ee7b7").
predefined_colour("emerald-400", "34d399").
predefined_colour("emerald-500", "10b981").
predefined_colour("emerald-600", "059669").
predefined_colour("emerald-700", "047857").
predefined_colour("emerald-800", "065f46").
predefined_colour("emerald-900", "064e3b").
predefined_colour("emerald-50", "ecfdf5").

predefined_colour("green-100", "dcfce7").
predefined_colour("green-200", "bbf7d0").
predefined_colour("green-300", "86efac").
predefined_colour("green-400", "4ade80").
predefined_colour("green-500", "22c55e").
predefined_colour("green-600", "16a34a").
predefined_colour("green-700", "15803d").
predefined_colour("green-800", "166534").
predefined_colour("green-900", "14532d").
predefined_colour("green-50", "f0fdf4").

predefined_colour("lime-100", "ecfccb").
predefined_colour("lime-200", "d9f99d").
predefined_colour("lime-300", "bef264").
predefined_colour("lime-400", "a3e635").
predefined_colour("lime-500", "84cc16").
predefined_colour("lime-600", "65a30d").
predefined_colour("lime-700", "4d7c0f").
predefined_colour("lime-800", "3f6212").
predefined_colour("lime-900", "365314").
predefined_colour("lime-50", "f7fee7").

predefined_colour("yellow-100", "fef9c3").
predefined_colour("yellow-200", "fef08a").
predefined_colour("yellow-300", "fde047").
predefined_colour("yellow-400", "facc15").
predefined_colour("yellow-500", "eab308").
predefined_colour("yellow-600", "ca8a04").
predefined_colour("yellow-700", "a16207").
predefined_colour("yellow-800", "854d0e").
predefined_colour("yellow-900", "713f12").
predefined_colour("yellow-50", "fefce8").

predefined_colour("amber-100", "fef3c7").
predefined_colour("amber-200", "fde68a").
predefined_colour("amber-300", "fcd34d").
predefined_colour("amber-400", "fbbf24").
predefined_colour("amber-500", "f59e0b").
predefined_colour("amber-600", "d97706").
predefined_colour("amber-700", "b45309").
predefined_colour("amber-800", "92400e").
predefined_colour("amber-900", "78350f").
predefined_colour("amber-50", "fffbeb").

predefined_colour("orange-100", "ffedd5").
predefined_colour("orange-200", "fed7aa").
predefined_colour("orange-300", "fdba74").
predefined_colour("orange-400", "fb923c").
predefined_colour("orange-500", "f97316").
predefined_colour("orange-600", "ea580c").
predefined_colour("orange-700", "c2410c").
predefined_colour("orange-800", "9a3412").
predefined_colour("orange-900", "7c2d12").
predefined_colour("orange-50", "fff7ed").

predefined_colour("red-100", "fee2e2").
predefined_colour("red-200", "fecaca").
predefined_colour("red-300", "fca5a5").
predefined_colour("red-400", "f87171").
predefined_colour("red-500", "ef4444").
predefined_colour("red-600", "dc2626").
predefined_colour("red-700", "b91c1c").
predefined_colour("red-800", "991b1b").
predefined_colour("red-900", "7f1d1d").
predefined_colour("red-50", "fef2f2").

predefined_colour("warmGray-100", "f5f5f4").
predefined_colour("warmGray-200", "e7e5e4").
predefined_colour("warmGray-300", "d6d3d1").
predefined_colour("warmGray-400", "a8a29e").
predefined_colour("warmGray-500", "78716c").
predefined_colour("warmGray-600", "57534e").
predefined_colour("warmGray-700", "44403c").
predefined_colour("warmGray-800", "292524").
predefined_colour("warmGray-900", "1c1917").
predefined_colour("warmGray-50", "fafaf9").

predefined_colour("trueGray-100", "f5f5f5").
predefined_colour("trueGray-200", "e5e5e5").
predefined_colour("trueGray-300", "d4d4d4").
predefined_colour("trueGray-400", "a3a3a3").
predefined_colour("trueGray-500", "737373").
predefined_colour("trueGray-600", "525252").
predefined_colour("trueGray-700", "404040").
predefined_colour("trueGray-800", "262626").
predefined_colour("trueGray-900", "171717").
predefined_colour("trueGray-50", "fafafa").

predefined_colour("gray-100", "f4f4f5").
predefined_colour("gray-200", "e4e4e7").
predefined_colour("gray-300", "d4d4d8").
predefined_colour("gray-400", "a1a1aa").
predefined_colour("gray-500", "71717a").
predefined_colour("gray-600", "52525b").
predefined_colour("gray-700", "3f3f46").
predefined_colour("gray-800", "27272a").
predefined_colour("gray-900", "18181b").
predefined_colour("gray-50", "fafafa").

predefined_colour("coolGray-100", "f3f4f6").
predefined_colour("coolGray-200", "e5e7eb").
predefined_colour("coolGray-300", "d1d5db").
predefined_colour("coolGray-400", "9ca3af").
predefined_colour("coolGray-500", "6b7280").
predefined_colour("coolGray-600", "4b5563").
predefined_colour("coolGray-700", "374151").
predefined_colour("coolGray-800", "1f2937").
predefined_colour("coolGray-900", "111827").
predefined_colour("coolGray-50", "f9fafb").

predefined_colour("blueGray-100", "f1f5f9").
predefined_colour("blueGray-200", "e2e8f0").
predefined_colour("blueGray-300", "cbd5e1").
predefined_colour("blueGray-400", "94a3b8").
predefined_colour("blueGray-500", "64748b").
predefined_colour("blueGray-600", "475569").
predefined_colour("blueGray-700", "334155").
predefined_colour("blueGray-800", "1e293b").
predefined_colour("blueGray-900", "0f172a").
predefined_colour("blueGray-50", "f8fafc").

rgb_values([R, G, B], R, G, B).
rgb_values([R0, R1, G0, G1, B0, B1], R, G, B) :-
    R is R0 * 16 + R1,
    G is G0 * 16 + G1,
    B is B0 * 16 + B1.
rgba_values([R, G, B, A], R, G, B, A).
rgba_values([R0, R1, G0, G1, B0, B1, A0, A1], R, G, B, A) :-
    R is R0 * 16 + R1,
    G is G0 * 16 + G1,
    B is B0 * 16 + B1,
    A is A0 * 16 + A1.

%! colour(-Colour)// is det.
%
%  DCG for parsing a colour in one of the many formats supported by
%  Tailwind/Girouette -- a preset list of colours, rgb, rgba, hsl,
%  hsla.
colour(rgba(R, G, B, A)) -->
    optional("#", "rgba-"), xdigits(Digits),
    { length(Digits, Len),
      ( Len == 4 -> true ; Len == 8 ),
      rgba_values(Digits, R, G ,B, A) }, !.
colour(rgb(R, G, B)) -->
    optional("#", "rgb-"), xdigits(Digits),
    { length(Digits, Len),
      ( Len == 3 -> true ; Len == 6 ),
      rgb_values(Digits, R, G ,B) }, !.

colour(hsla(H, S, L, A)) -->
    "hsla-", number(H), "-", number(S), "-", number(L), "-", number(A).
colour(hsl(H, S, L)) -->
    "hsl-", number(H), "-", number(S), "-", number(L).

colour(special(transparent)) --> "transparent", !.
colour(special(currentColour)) --> "current", !.

colour(rgba(R, G, B, A)) -->
    predef_colour(rgb(R, G, B)), "-", number(Opacity), !,
    { A is max(0, min(255, (Opacity * 255) // 100)) }.
colour(Colour) --> predef_colour(Colour), !.

%! has_alpha(+Colour) is semidet.
%
%  Predicate that succeeds when =Colour= has an alpha component.
has_alpha(rgba(_, _, _, _)).
has_alpha(hsla(_, _, _, _)).

colour_with_alpha(rgb(R, G, B), Alpha, rgba(R, G, B, Alpha)).
colour_with_alpha(hsl(H, S, L), Alpha255, hsla(H, S, L, Alpha)) :-
    format(string(Alpha), "calc(~w / 255)", [Alpha255]).

%! as_transparent(+Colour0, -Colour1) is det.
%
%  True when =Colour1= is =Colour0= but with an alpha channel
%  specified.
as_transparent(rgb(R, G, B), rgba(R, G, B, 0)) :- !.
as_transparent(rgba(R, G, B, _), rgba(R, G, B, 0)) :- !.
as_transparent(hsl(H, S, L), hsla(H, S, L, 0)) :- !.
as_transparent(hsla(H, S, L, _), hsla(H, S, L, 0)) :- !.
as_transparent(Colour, Colour).

%! colour_css(+Colour, -CssStr) is det.
%
%  Unifies =CssStr= with the CSS string representation of =Colour=.
colour_css(rgb(R, G, B), S) :-
    format(string(S), "#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+", [R, G, B]).
colour_css(rgba(R, G, B, A), S) :-
    string(A), !,
    format(string(S), "rgba(~w, ~w, ~w, ~w)", [R, G, B, A]).
colour_css(rgba(R, G, B, A), S) :-
    format(string(S), "#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+",
           [R, G, B, A]).
colour_css(hsl(H, S, L), S) :-
    format(string(S), "hsl(~w, ~w, ~w)", [H, S, L]).
colour_css(hsla(H, S, L, A), S) :-
    format(string(S), "hsla(~w, ~w, ~w, ~w)", [H, S, L, A]).
