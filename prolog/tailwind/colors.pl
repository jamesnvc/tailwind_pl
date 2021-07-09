:- module(colors, [color//1,
                   has_alpha/1,
                   color_with_alpha/3,
                   as_transparent/2,
                   color_css/2]).

:- use_module(library(dcg/basics), [xdigits//1, number//1]).
:- use_module(library(dcg/high_order), [optional//2]).

:- discontiguous color//1.

% pre-defined colours

term_expansion(predefined_color(Name, RGB), DcgDef) :-
    string_codes(Name, NameCodes),
    hex_bytes(RGB, [R, G, B]),
    expand_term((predef_color(rgb(R, G, B)) --> NameCodes, !),
                DcgDef).

predefined_color("white", "ffffff").
predefined_color("black", "000000").

predefined_color("rose-100", "ffe4e6").
predefined_color("rose-200", "fecdd3").
predefined_color("rose-300", "fda4af").
predefined_color("rose-400", "fb7185").
predefined_color("rose-500", "f43f5e").
predefined_color("rose-600", "e11d48").
predefined_color("rose-700", "be123c").
predefined_color("rose-800", "9f1239").
predefined_color("rose-900", "881337").
predefined_color("rose-50", "fff1f2").

predefined_color("pink-100", "fce7f3").
predefined_color("pink-200", "fbcfe8").
predefined_color("pink-300", "f9a8d4").
predefined_color("pink-400", "f472b6").
predefined_color("pink-500", "ec4899").
predefined_color("pink-600", "db2777").
predefined_color("pink-700", "be185d").
predefined_color("pink-800", "9d174d").
predefined_color("pink-900", "831843").
predefined_color("pink-50", "fdf2f8").

predefined_color("fuchsia-100", "fae8ff").
predefined_color("fuchsia-200", "f5d0fe").
predefined_color("fuchsia-300", "f0abfc").
predefined_color("fuchsia-400", "e879f9").
predefined_color("fuchsia-500", "d946ef").
predefined_color("fuchsia-600", "c026d3").
predefined_color("fuchsia-700", "a21caf").
predefined_color("fuchsia-800", "86198f").
predefined_color("fuchsia-900", "701a75").
predefined_color("fuchsia-50", "fdf4ff").

predefined_color("purple-100", "f3e8ff").
predefined_color("purple-200", "e9d5ff").
predefined_color("purple-300", "d8b4fe").
predefined_color("purple-400", "c084fc").
predefined_color("purple-500", "a855f7").
predefined_color("purple-600", "9333ea").
predefined_color("purple-700", "7e22ce").
predefined_color("purple-800", "6b21a8").
predefined_color("purple-900", "581c87").
predefined_color("purple-50", "faf5ff").

predefined_color("violet-100", "ede9fe").
predefined_color("violet-200", "ddd6fe").
predefined_color("violet-300", "c4b5fd").
predefined_color("violet-400", "a78bfa").
predefined_color("violet-500", "8b5cf6").
predefined_color("violet-600", "7c3aed").
predefined_color("violet-700", "6d28d9").
predefined_color("violet-800", "5b21b6").
predefined_color("violet-900", "4c1d95").
predefined_color("violet-50", "f5f3ff").

predefined_color("indigo-100", "e0e7ff").
predefined_color("indigo-200", "c7d2fe").
predefined_color("indigo-300", "a5b4fc").
predefined_color("indigo-400", "818cf8").
predefined_color("indigo-500", "6366f1").
predefined_color("indigo-600", "4f46e5").
predefined_color("indigo-700", "4338ca").
predefined_color("indigo-800", "3730a3").
predefined_color("indigo-900", "312e81").
predefined_color("indigo-50", "eef2ff").

predefined_color("blue-100", "dbeafe").
predefined_color("blue-200", "bfdbfe").
predefined_color("blue-300", "93c5fd").
predefined_color("blue-400", "60a5fa").
predefined_color("blue-500", "3b82f6").
predefined_color("blue-600", "2563eb").
predefined_color("blue-700", "1d4ed8").
predefined_color("blue-800", "1e40af").
predefined_color("blue-900", "1e3a8a").
predefined_color("blue-50", "eff6ff").

predefined_color("lightBlue-100", "e0f2fe").
predefined_color("lightBlue-200", "bae6fd").
predefined_color("lightBlue-300", "7dd3fc").
predefined_color("lightBlue-400", "38bdf8").
predefined_color("lightBlue-500", "0ea5e9").
predefined_color("lightBlue-600", "0284c7").
predefined_color("lightBlue-700", "0369a1").
predefined_color("lightBlue-800", "075985").
predefined_color("lightBlue-900", "0c4a6e").
predefined_color("lightBlue-50", "f0f9ff").

predefined_color("cyan-100", "cffafe").
predefined_color("cyan-200", "a5f3fc").
predefined_color("cyan-300", "67e8f9").
predefined_color("cyan-400", "22d3ee").
predefined_color("cyan-500", "06b6d4").
predefined_color("cyan-600", "0891b2").
predefined_color("cyan-700", "0e7490").
predefined_color("cyan-800", "155e75").
predefined_color("cyan-900", "164e63").
predefined_color("cyan-50", "ecfeff").

predefined_color("teal-100", "ccfbf1").
predefined_color("teal-200", "99f6e4").
predefined_color("teal-300", "5eead4").
predefined_color("teal-400", "2dd4bf").
predefined_color("teal-500", "14b8a6").
predefined_color("teal-600", "0d9488").
predefined_color("teal-700", "0f766e").
predefined_color("teal-800", "115e59").
predefined_color("teal-900", "134e4a").
predefined_color("teal-50", "f0fdfa").

predefined_color("emerald-100", "d1fae5").
predefined_color("emerald-200", "a7f3d0").
predefined_color("emerald-300", "6ee7b7").
predefined_color("emerald-400", "34d399").
predefined_color("emerald-500", "10b981").
predefined_color("emerald-600", "059669").
predefined_color("emerald-700", "047857").
predefined_color("emerald-800", "065f46").
predefined_color("emerald-900", "064e3b").
predefined_color("emerald-50", "ecfdf5").

predefined_color("green-100", "dcfce7").
predefined_color("green-200", "bbf7d0").
predefined_color("green-300", "86efac").
predefined_color("green-400", "4ade80").
predefined_color("green-500", "22c55e").
predefined_color("green-600", "16a34a").
predefined_color("green-700", "15803d").
predefined_color("green-800", "166534").
predefined_color("green-900", "14532d").
predefined_color("green-50", "f0fdf4").

predefined_color("lime-100", "ecfccb").
predefined_color("lime-200", "d9f99d").
predefined_color("lime-300", "bef264").
predefined_color("lime-400", "a3e635").
predefined_color("lime-500", "84cc16").
predefined_color("lime-600", "65a30d").
predefined_color("lime-700", "4d7c0f").
predefined_color("lime-800", "3f6212").
predefined_color("lime-900", "365314").
predefined_color("lime-50", "f7fee7").

predefined_color("yellow-100", "fef9c3").
predefined_color("yellow-200", "fef08a").
predefined_color("yellow-300", "fde047").
predefined_color("yellow-400", "facc15").
predefined_color("yellow-500", "eab308").
predefined_color("yellow-600", "ca8a04").
predefined_color("yellow-700", "a16207").
predefined_color("yellow-800", "854d0e").
predefined_color("yellow-900", "713f12").
predefined_color("yellow-50", "fefce8").

predefined_color("amber-100", "fef3c7").
predefined_color("amber-200", "fde68a").
predefined_color("amber-300", "fcd34d").
predefined_color("amber-400", "fbbf24").
predefined_color("amber-500", "f59e0b").
predefined_color("amber-600", "d97706").
predefined_color("amber-700", "b45309").
predefined_color("amber-800", "92400e").
predefined_color("amber-900", "78350f").
predefined_color("amber-50", "fffbeb").

predefined_color("orange-100", "ffedd5").
predefined_color("orange-200", "fed7aa").
predefined_color("orange-300", "fdba74").
predefined_color("orange-400", "fb923c").
predefined_color("orange-500", "f97316").
predefined_color("orange-600", "ea580c").
predefined_color("orange-700", "c2410c").
predefined_color("orange-800", "9a3412").
predefined_color("orange-900", "7c2d12").
predefined_color("orange-50", "fff7ed").

predefined_color("red-100", "fee2e2").
predefined_color("red-200", "fecaca").
predefined_color("red-300", "fca5a5").
predefined_color("red-400", "f87171").
predefined_color("red-500", "ef4444").
predefined_color("red-600", "dc2626").
predefined_color("red-700", "b91c1c").
predefined_color("red-800", "991b1b").
predefined_color("red-900", "7f1d1d").
predefined_color("red-50", "fef2f2").

predefined_color("warmGray-100", "f5f5f4").
predefined_color("warmGray-200", "e7e5e4").
predefined_color("warmGray-300", "d6d3d1").
predefined_color("warmGray-400", "a8a29e").
predefined_color("warmGray-500", "78716c").
predefined_color("warmGray-600", "57534e").
predefined_color("warmGray-700", "44403c").
predefined_color("warmGray-800", "292524").
predefined_color("warmGray-900", "1c1917").
predefined_color("warmGray-50", "fafaf9").

predefined_color("trueGray-100", "f5f5f5").
predefined_color("trueGray-200", "e5e5e5").
predefined_color("trueGray-300", "d4d4d4").
predefined_color("trueGray-400", "a3a3a3").
predefined_color("trueGray-500", "737373").
predefined_color("trueGray-600", "525252").
predefined_color("trueGray-700", "404040").
predefined_color("trueGray-800", "262626").
predefined_color("trueGray-900", "171717").
predefined_color("trueGray-50", "fafafa").

predefined_color("gray-100", "f4f4f5").
predefined_color("gray-200", "e4e4e7").
predefined_color("gray-300", "d4d4d8").
predefined_color("gray-400", "a1a1aa").
predefined_color("gray-500", "71717a").
predefined_color("gray-600", "52525b").
predefined_color("gray-700", "3f3f46").
predefined_color("gray-800", "27272a").
predefined_color("gray-900", "18181b").
predefined_color("gray-50", "fafafa").

predefined_color("coolGray-100", "f3f4f6").
predefined_color("coolGray-200", "e5e7eb").
predefined_color("coolGray-300", "d1d5db").
predefined_color("coolGray-400", "9ca3af").
predefined_color("coolGray-500", "6b7280").
predefined_color("coolGray-600", "4b5563").
predefined_color("coolGray-700", "374151").
predefined_color("coolGray-800", "1f2937").
predefined_color("coolGray-900", "111827").
predefined_color("coolGray-50", "f9fafb").

predefined_color("blueGray-100", "f1f5f9").
predefined_color("blueGray-200", "e2e8f0").
predefined_color("blueGray-300", "cbd5e1").
predefined_color("blueGray-400", "94a3b8").
predefined_color("blueGray-500", "64748b").
predefined_color("blueGray-600", "475569").
predefined_color("blueGray-700", "334155").
predefined_color("blueGray-800", "1e293b").
predefined_color("blueGray-900", "0f172a").
predefined_color("blueGray-50", "f8fafc").

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

color(rgba(R, G, B, A)) -->
    optional("#", "rgba-"), xdigits(Digits),
    { length(Digits, Len),
      ( Len == 4 -> true ; Len == 8 ),
      rgba_values(Digits, R, G ,B, A) }, !.
color(rgb(R, G, B)) -->
    optional("#", "rgb-"), xdigits(Digits),
    { length(Digits, Len),
      ( Len == 3 -> true ; Len == 6 ),
      rgb_values(Digits, R, G ,B) }, !.

color(hsla(H, S, L, A)) -->
    "hsla-", number(H), "-", number(S), "-", number(L), "-", number(A).
color(hsl(H, S, L)) -->
    "hsl-", number(H), "-", number(S), "-", number(L).

color(special(transparent)) --> "transparent", !.
color(special(currentColor)) --> "current", !.

color(rgba(R, G, B, A)) -->
    predef_color(rgb(R, G, B)), "-", number(Opacity), !,
    { A is max(0, min(255, (Opacity * 255) // 100)) }.
color(Color) --> predef_color(Color), !.

has_alpha(rgba(_, _, _, _)).
has_alpha(hsla(_, _, _, _)).

color_with_alpha(rgb(R, G, B), Alpha, rgba(R, G, B, Alpha)).
color_with_alpha(hsl(H, S, L), Alpha255, hsla(H, S, L, Alpha)) :-
    format(string(Alpha), "calc(~w / 255)", [Alpha255]).

as_transparent(rgb(R, G, B), rgba(R, G, B, 0)) :- !.
as_transparent(rgba(R, G, B, _), rgba(R, G, B, 0)) :- !.
as_transparent(hsl(H, S, L), hsla(H, S, L, 0)) :- !.
as_transparent(hsla(H, S, L, _), hsla(H, S, L, 0)) :- !.
as_transparent(Color, Color).

color_css(rgb(R, G, B), S) :-
    format(string(S), "#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+", [R, G, B]).
color_css(rgba(R, G, B, A), S) :-
    string(A), !,
    format(string(S), "rgba(~w, ~w, ~w, ~w)", [R, G, B, A]).
color_css(rgba(R, G, B, A), S) :-
    format(string(S), "#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+",
           [R, G, B, A]).
color_css(hsl(H, S, L), S) :-
    format(string(S), "hsl(~w, ~w, ~w)", [H, S, L]).
color_css(hsla(H, S, L, A), S) :-
    format(string(S), "hsla(~w, ~w, ~w, ~w)", [H, S, L, A]).
