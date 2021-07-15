:- module(grammar, [tailwind//1, prefixes//2]).
/** <module> Tailwind CSS grammar

DCGs for parsing Tailwind selectors to the corresponding CSS

@author James Cash
*/

:- use_module(library(apply_macros)).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(dcg/basics), [integer//1, string_without//2]).
:- use_module(library(dcg/high_order), [optional//2]).
:- use_module(library(yall)).

:- use_module(tw_utils).
:- use_module(colours, [colour//1,
                        has_alpha/1,
                        colour_css/2,
                        as_transparent/2,
                        colour_with_alpha/3]).

%! prefixes(-MediaQueries, -StateVariants)// is det.
%
%  DCG to parse possibly-empty media query & state variant prefixes of
%  a tailwind selector; e.g. in "lg:hover:text-blue" would have
%  =MediaQueries= be [min_width("1024px")] and =StateVariants= be
%  [inner(hover)].
prefixes(Medias, States) -->
    media_queries(Medias), state_variants(States).

media_queries([Media|Medias]) -->
    media_query(Media), ":", !,
    media_queries(Medias).
media_queries([]) --> [].

media_query(min_width("640px")) --> "sm".
media_query(min_width("768px")) --> "md".
media_query(min_width("1024px")) --> "lg".
media_query(min_width("1280px")) --> "xl".
media_query(min_width("1536")) --> "2xl".

media_query(color_scheme(light)) --> "light".
media_query(color_scheme(dark)) --> "dark".

media_query(motion("no-preference")) --> "motion-safe".
media_query(motion("reduced")) --> "motion-reduce".

state_variants([State|States]) -->
    state_variant(State), ":", !,
    state_variants(States).
state_variants([]) --> [].

state_variant(outer(Variant)) -->
    "group-", one_of(["hover", "focus", "disabled", "active"],
                     GroupVariant), !,
    { format(atom(Variant), ".group:~w", [GroupVariant]) }.
state_variant(outer(Variant)) -->
    "group-attr-", string_without(":", Attr), !,
    { format(atom(Variant), ".group[~s]", [Attr]) }.
state_variant(inner(Variant)) -->
    one_of(
        ["hover", "focus", "disabled", "active",
         "focus-within", "focus-visible",
         "any-link", "link", "visited", "target",
         "blank", "required", "optionaly", "valid", "invalid",
         "placeholder-shown", "checked", "read-only", "read-write",
         % need to make sure prefixes are last, since one_of is greedy
         "first-of-type", "last-of-type", "root", "empty"],
        Variant).
state_variant(inner("first-child")) --> "first", !.
state_variant(inner("last-child")) --> "last", !.
state_variant(inner("nth-child(odd)")) --> "odd", !.
state_variant(inner("nth-child(even)")) --> "even", !.

:- discontiguous tailwind//1.

% Flex

%! tailwind(Styles)// is semidet.
%
%  DCG to parse the many possible Tailwind selectors.
tailwind('flex-grow'(V)) -->
    "flex-grow-", optional(fraction(N), num(N)),
    { value_unit_css(N, V, _{}) }.
tailwind('flex-grow'(1)) --> "flex-grow".

tailwind('flex-shrink'(V)) -->
    "flex-shrink-", optional(fraction(N), num(N)),
    { value_unit_css(N, V, _{}) }.
tailwind('flex-shrink'(1)) --> "flex-shrink".

flex_basis_value(Val) --> fraction(Val).
flex_basis_value(Val) --> percentage(Val).
flex_basis_value(Val) --> length(Val).
flex_basis_value(Val) --> length_unit(Val).
flex_basis_value(Val) --> num(Val).
flex_basis_value(full_100) --> "full".
flex_basis_value(auto) --> "auto".

tailwind('flex-basis'(N)) -->
    "flex-basis-", flex_basis_value(Val), !,
    { value_unit_css(Val, N, _{zero: "",
                              number: _{unit: rem,
                                        value_fn: div_4},
                              fraction: _{unit: '%',
                                          value_fn: mul_100}}) }.
tailwind('flex-basis'(1)) --> "flex-basis", !.

tailwind('flex'("none")) --> "flex-none", !.
tailwind('flex'("0 1 auto")) --> "flex-initial", !.
tailwind('flex'("1 1 auto")) --> "flex-auto", !.
tailwind('flex'(S)) -->
    "flex-",
    optional(fraction(GrowN), num(GrowN)),
    "-", optional(fraction(ShrinkN), num(ShrinkN)),
    "-", flex_basis_value(BasisN), !,
    { value_unit_css(GrowN, Grow, _{}),
      value_unit_css(ShrinkN, Shrink, _{}),
      value_unit_css(BasisN, Basis, _{zero_unit: "",
                                      number: _{unit: "rem",
                                                value_fn: div_4},
                                      fraction: _{unit: "%",
                                                  value_fn: mul_100}
                                     }),
      format(string(S), "~w ~w ~w", [Grow, Shrink, Basis]) }.
tailwind('flex'(S)) -->
    "flex-",
    optional(fraction(GrowN), num(GrowN)),
    "-", flex_basis_value(ShrinkOrBasisN), !,
    { value_unit_css(GrowN, Grow, _{}),
      value_unit_css(ShrinkOrBasisN, ShrinkOrBasis, _{}),
      format(string(S), "~w ~w", [Grow, ShrinkOrBasis]) }.
tailwind('flex'(S)) -->
    "flex-", optional(fraction(N), num(N)), !,
    { value_unit_css(N, CssN, _{}),
      format(string(S), "~w ~w 0%", [CssN, CssN]) }, !.

tailwind('flex-direction'("row-reverse")) --> "flex-row-reverse", !.
tailwind('flex-direction'("row")) --> "flex-row", !.
tailwind('flex-direction'("column-reverse")) --> "flex-col-reverse", !.
tailwind('flex-direction'("column")) --> "flex-col", !.

tailwind('flex-wrap'(Wrap)) -->
    one_of(["wrap-reverse", "wrap", "nowrap"], Wrap).

tailwind(order(-9999)) --> "order-first", !.
tailwind(order(9999)) --> "order-last", !.
tailwind(order(0)) --> "order-none", !.
tailwind(order(O)) -->
    soft_optional(signus(S), { S = '+' }),
    "order-",
    integer(N), !,
    { value_unit_css(integer(N), O, _{signus: S }) }.

% Accessibility

tailwind([position(absolute), width("1px"), height("1px"),
          padding(0), margin("-1px"), overflow(hidden),
          clip("rect(0,0,0,0)"), 'white-space'(nowrap),
          'border-width'(0)]) -->
    "sr-only", !.
tailwind([position(static), width(auto), height(auto),
          padding(0), margin(0), overflow(visible),
          clip(auto), 'white-space'(normal)]) -->
    "not-sr-only", !.

% Animation

tailwind('transition-property'(none)) --> "tranistion-none", !.
tailwind(['transition-property'(Prop),
          'transition-timing-function'("cubic-bezier(0.4,0,0.2.1)"),
          'transition-duration'("150ms")]) -->
    "transition-",
    transition_property(Prop), !.
tailwind(['transition-property'("background-color,border-color,color,fill,stroke,opacity,box-shadow,transform"),
          'transition-timing-function'("cubic-bezier(0.4,0,0.2.1)"),
          'transition-duration'("150ms")]) -->
    "transition".

transition_property("all") --> "all".
transition_property("background-color,border-color,color,fill-stroke") -->
    "colors".
transition_property("opacity") --> "opacity".
transition_property("box-shadow") --> "shadow".
transition_property("transform") --> "transform".

tailwind('transition-duration'(Dur)) -->
    "duration-", optional(time(N), num(N)), !,
    { value_unit_css(N, Dur, _{zero_unit: s, number: _{unit: ms}})}.

tailwind('transition-timing-function'(Fun)) -->
    "ease-", transition_timing_fn(Fun), !.

transition_timing_fn("linear") --> "linear".
transition_timing_fn("cubic-bezier(0.4,0,0.2,1)") --> "in-out".
transition_timing_fn("cubic-bezier(0.4,0,1,1)") --> "in".
transition_timing_fn("cubic-bezier(0,0,0.2,1)") --> "out".

tailwind('transition-delay'(Delay)) -->
    "delay-", optional(time(N), num(N)), !,
    { value_unit_css(N, Delay, _{zero_unit: s, number: _{unit: ms}}) }.

tailwind('animation'("none")) --> "animate-none", !.
tailwind(['animation'("spin 1s linear infinite"),
         '@keyframes'(spin,
                      [from(transform("rotate(0)")),
                       to(transform("rotate(360deg)"))])]) -->
    "animate-spin", !.
tailwind(['animation'("ping 1s cubic-bezier(0,0,0.2,1) infinite"),
         '@keyframes'(ping,
                      ['75%, 100%'(transform("scale(2)"),
                                   opacity(0))])]) -->
    "animate-ping", !.
tailwind(['animation'("pulse 2s cubic-bezier(0.4,0,0.6,1) infinite"),
         '@keyframes'(pulse,
                      ['0%, 100%'(opacity(0)),
                       '50%'(opacity(0.5))])]) -->
    "animate-pulse", !.
tailwind(['animation'("bounce 1s infinite"),
         '@keyframes'(bounce,
                      ['0%, 100%'(transform("translateY(-25%)"),
                                 'animation-timing-function'("cubic-bezier(0.8,0,1,1)")),
                       '50%'(transform("translateY(0)"),
                             'animation-timing-function'("cubic-bezier(0,0,0.2,1)"))])]) -->
    "animate-bounce", !.

% Background

tailwind('background-attachment'(Attachment)) -->
    "bg-", one_of(["fixed", "local", "scroll"],
                  Attachment), !.

tailwind('background-clip'("text")) --> "bg-clip-text", !.
tailwind('background-clip'(Box)) -->
    "bg-clip-",
    one_of(["border", "padding", "content"], Type), !,
    { format(string(Box), "~w-box", [Type]) }.

colour_bg(special(S), S) :- !.
colour_bg(Colour, CssColour) :-
    ( has_alpha(Colour)
    -> Colour_ = Colour
    ;  colour_with_alpha(Colour, "var(--pl-bg-opacity, 1)", Colour_) ),
    colour_css(Colour_, CssColour).

tailwind('background-color'(CssColour)) -->
    "bg-", colour(Colour), !,
    { colour_bg(Colour, CssColour) }.

tailwind('--pl-bg-opacity'(Opacity)) -->
    "bg-opacity-", num(N), !,
    { value_unit_css(N, Opacity, _{value_fn: div_100}) }.

tailwind('background-position'("right top")) --> "bg-right-top".
tailwind('background-position'("left top")) --> "bg-left-top".
tailwind('background-position'("right bottom")) --> "bg-right-bottom".
tailwind('background-position'("left bottom")) --> "bg-left-bottom".
tailwind('background-position'(Pos)) -->
    "bg-", one_of(["top", "center", "bottom", "left", "right"], Pos).

background_size_length(Len) -->
    alternates([auto(Len),
               length(Len), length_unit(Len),
               fraction(Len), percentage(Len),
               num(Len)]).

tailwind('background-size'(Size)) -->
    "bg-", one_of(["auto", "cover", "contain"], Size), !.
tailwind('background-size'(Size)) -->
    "bg-size-", background_size_length(Width),
    "-", background_size_length(Height), !,
    { Opts = _{zero_unit: "",
               number: _{unit: rem, value_fn: div_4},
               fraction: _{unit: "%", value_fn: mul_100}},
        value_unit_css(Width, WidthCss, Opts),
        value_unit_css(Height, HeightCss, Opts),
        format(string(Size), "~w ~w", [WidthCss, HeightCss]) }.

gradient_direction("top right") --> "tr".
gradient_direction("top left") --> "tl".
gradient_direction("top") --> "t".
gradient_direction("left") --> "l".
gradient_direction("right") --> "r".
gradient_direction("bottom right") --> "br".
gradient_direction("bottom left") --> "bl".
gradient_direction("bottom") --> "b".

tailwind('background-image'("none")) --> "bg-none", !.
tailwind('background-image'(Grad)) -->
    "bg-gradient-to-", gradient_direction(Dir), !,
    { format(string(Grad), "linear-gradient(to ~w, var(--pl-gradient-stops))",
             [Dir]) }.

tailwind(['--pl-gradient-from'(ColourCss),
          '--pl-gradient-stops'(Stops)]) -->
    "from-", colour(Colour), !,
    { colour_css(Colour, ColourCss),
      as_transparent(Colour, TranspColour), colour_css(TranspColour, TranspColourCss),
      format(string(Stops),
            "var(--pl-gradient-from), var(--pl-gradient-to, ~w)",
            [TranspColourCss]) }.

tailwind('--pl-gradient-to'(ColourCss)) -->
    "to-", colour(Colour), !,
    { colour_css(Colour, ColourCss) }.

tailwind('--pl-gradient-stops'(Stops)) -->
    "via-", colour(Colour), !,
    { as_transparent(Colour, ColourTransp),
      colour_css(Colour, ColourCss),
      colour_css(ColourTransp, ColourTranspCss),
      format(string(Stops),
            "var(--pl-gradient-from),~w,var(--pl-gradient-to,~w)",
            [ColourCss, ColourTranspCss]) }.

% Border

border_radius_pos(Pos) -->
    "-", one_of(["tl", "tr", "t",
                 "bl", "br", "b",
                 "r", "l"],
                Pos_),
    { border_radius_pos_dirs(Pos_, Pos) }.

border_radius_pos_dirs(t, ["top-left", "top-right"]).
border_radius_pos_dirs(r, ["top-right", "bottom-right"]).
border_radius_pos_dirs(b, ["bottom-right", "bottom-left"]).
border_radius_pos_dirs(l, ["bottom-left", "top-left"]).
border_radius_pos_dirs(tl, ["top-left"]).
border_radius_pos_dirs(tr, ["top-right"]).
border_radius_pos_dirs(br, ["bottom-right"]).
border_radius_pos_dirs(bl, ["bottom-left"]).

border_radius_size("0px") --> "-none", !.
border_radius_size("9999px") --> "-full", !.
border_radius_size(Size) -->
    "-", border_radius_size_multi(Cls),
    { S is Cls * 0.25,
      format(string(Size), "~wrem", [S]) }.

border_radius_size_multi(0.5) --> "sm", !.
border_radius_size_multi(1.5) --> "md", !.
border_radius_size_multi(2) --> "lg", !.
border_radius_size_multi(3) --> "xl", !.
border_radius_size_multi(4) --> "2xl", !.
border_radius_size_multi(6) --> "3xl".


tailwind(Style) -->
    "rounded",
    optional(border_radius_pos(Pos), { Pos = false}),
    optional(border_radius_size(Size), { Size = "0.25rem"}), !,
    { Pos == false
      -> Style = 'border-radius'(Size)
      ;   maplist({Size}/[D, St]>>(
                      format(atom(Attr), "border-~w-radius", [D]),
                      St =.. [Attr, Size]
                  ),
                  Pos,
                  Style) }.

colour_border(special(S), S) :- !.
colour_border(Colour, CssColour) :-
    ( has_alpha(Colour)
    -> Colour_ = Colour
    ;  colour_with_alpha(Colour, "var(--pl-border-opacity,1)", Colour_) ),
    colour_css(Colour_, CssColour).

tailwind('border-color'(ColourCss)) -->
    "border-", colour(Colour), !,
    { colour_border(Colour, ColourCss) }.

tailwind('--pl-border-opacity'(Opacity)) -->
    "border-opacity-", num(Num), !,
    { value_unit_css(Num, Opacity, _{value_fn: div_100}) }.

tailwind('border-style'(Style)) -->
    "border-", one_of(["solid", "dashed", "dotted", "double", "none"], Style), !.

divide_width_value(WidthVal) -->
    "-", alternates([length(WidthVal), length_unit(WidthVal), num(WidthVal)]).

divide_axis_styles(x, Width, ['border-right-width'(RStyle),
                              'border-left-width'(LStyle)]) :-
    format(string(RStyle), "calc(~w * var(--pl-divide-x-reverse))", [Width]),
    format(string(LStyle), "calc(~w * calc(1 - var(--pl-divide-x-reverse)))", [Width]).
divide_axis_styles(y, Width, ['border-top-width'(TStyle),
                              'border-bottom-width'(BStyle)]) :-
    format(string(TStyle), "calc(~w * var(--pl-divide-y-reverse))", [Width]),
    format(string(BStyle), "calc(~w * calc(1 - var(--pl-divide-y-reverse)))", [Width]).

tailwind('&'('> * + *'(Styles))) --> % should the selector be * ~ *?
    "divide-", axis(axis(Axis)),
    optional(divide_width_value(WidthVal), { WidthVal = length(1,px) }), !,
    { value_unit_css(WidthVal, CssVal, _{zero_unit: "", number: _{unit: px}}),
      divide_axis_styles(Axis, CssVal, Styles) }.

direction_border_attr(t, 'border-top-width').
direction_border_attr(r, 'border-right-width').
direction_border_attr(b, 'border-bottom-width').
direction_border_attr(l, 'border-left-width').

colour_divide(special(S), S) :- !.
colour_divide(Colour, CssColour) :-
    ( has_alpha(Colour)
    -> Colour_ = Colour
    ;  colour_with_alpha(Colour, "var(--pl-divide-opacity,1)", Colour_) ),
    colour_css(Colour_, CssColour).

tailwind('&'('> * + *'('border-color'(ColourCss)))) -->
    "divide-", colour(Colour), !,
    { colour_divide(Colour, ColourCss) }.

tailwind('--pl-divide-opacity'(Opacity)) -->
    "divide-opacity-", num(Number), !,
    { value_unit_css(Number, Opacity, _{value_fn: div_100}) }.

tailwind('&'('> * + *'('border-style'(Style)))) -->
    "divide-", one_of(["solid", "dashed", "dotted", "double", "none"], Style), !.

ring_colour(special(Colour), Colour) :- !.
ring_colour(Colour, ColourCss) :-
    has_alpha(Colour), !,
    colour_css(Colour, ColourCss).
ring_colour(Colour, ColourCss) :-
    colour_with_alpha(Colour, "var(--pl-ring-opacity,1)", Colour_),
    colour_css(Colour_, ColourCss).

tailwind('--pl-ring-color'(ColourCss)) -->
    "ring-", colour(Colour), !,
    { ring_colour(Colour, ColourCss) }.

tailwind('--pl-ring-opacity'(Opacity)) -->
    "ring-opacity-", num(Number), !,
    { value_unit_css(Number, Opacity, _{value_fn: div_100}) }.

tailwind(['--pl-ring-offset-width'(OffWidth),
         'box-shadow'("0 0 0 var(--pl-ring-offset-width) var(--pl-ring-offset-color), var(--pl-ring-shadow)")]) -->
    "ring-offset-", alternates([length(X), length_unit(X), num(X)]), !,
    { value_unit_css(X, OffWidth, _{zero_unit: "", number: _{unit: px}}) }.

tailwind(['--pl-ring-offset-color'(OffColour),
          'box-shadow'("0 0 0 var(--pl-ring-offset-width, 0px) var(--pl-ring-offset-color), var(--pl-ring-shadow)")]) -->
    "ring-offset-", colour(Colour), !,
    { ring_colour(Colour, OffColour) }.

tailwind('--pl-ring-inset'("inset")) --> "ring-inset", !.
tailwind('box-shadow'(Style)) -->
    "ring-", alternates([length(Len), length_unit(Len), num(Len)]), !,
    { value_unit_css(Len, LenCss, _{zero_unit: "", number: _{unit: px}}),
      format(string(Style),
             "var(--pl-ring-inset, ) 0 0 0 calc(~w + var(--pl-ring-offset-width,0px)) var(--pl-ring-color)",
            [LenCss]) }.
% needs to come after all the other "ring" styles
tailwind('box-shadow'("var(--pl-ring-inset, ) 0 0 0 calc(3px + var(--pl-ring-offset-width,0px)) var(--pl-ring-color)")) -->
    "ring", !.

border_direction(Attr) -->
    "-", direction(direction(Dir)),
    { direction_border_attr(Dir, Attr) }.

border_width_val(Width) -->
    "-", alternates([length(Len), length_unit(Len), num(Len)]),
    { value_unit_css(Len, Width, _{zero_unit: "", number: _{unit: px}}) }.

% like optionaly, but don't cut on default
soft_optional(A, _) --> A, !.
soft_optional(_, B) --> B.

% not doing just one with optionals, because "border-rem" & "border-r"
% are both valid, but hard to parse properly without leaving
% choice-points
% this also needs to come after all the other "border-" cases, since
% it's a prefix of all of them
tailwind('border-width'(Width)) -->
    "border", border_width_val(Width), !.
tailwind(Style) -->
    "border",
    soft_optional(border_direction(Attr), { Attr = 'border-width' }),
    soft_optional(border_width_val(Width), { Width = "1px" }), !,
    { Style =.. [Attr, Width] }.

% Justify

flex_align_style(start, "flex-start").
flex_align_style(end, "flex-end").
flex_align_style(center, "center").
flex_align_style(between, "space-between").
flex_align_style(around, "space-around").
flex_align_style(evenly, "space-evenly").

tailwind('justify-content'(Justify)) -->
    "justify-", one_of(["start", "end", "center", "between", "around", "evenly"],
                      Just), !,
    { flex_align_style(Just, Justify) }.

tailwind('justify-items'(Justify)) -->
    "justify-items-",
    one_of(["auto", "start" , "end", "center", "stretch"], Justify), !.

tailwind('justify-self'(Justify)) -->
    "justify-self-",
    one_of(["auto", "start" , "end", "center", "stretch"], Justify), !.

tailwind('align-content'(Align)) -->
    "content-", one_of(["start", "end", "center", "between", "around", "evenly"],
                       A), !,
    { flex_align_style(A, Align) }.

flex_items_style(start, "flex-start") :- !.
flex_items_style(end, "flex-end") :- !.
flex_items_style(S, S).

tailwind('align-items'(Align)) -->
    "items-", one_of(["start", "end", "center", "baseline", "stretch"],
                     A), !,
    { flex_items_style(A, Align) }.

tailwind('align-self'(Align)) -->
    "self-", one_of(["start", "end", "center", "stretch", "auto"],
                     A), !,
    { flex_items_style(A, Align) }.

place_content_place(between, "space-between") :- !.
place_content_place(around, "space-around") :- !.
place_content_place(evenly, "space-evenly") :- !.
place_content_place(P, P).

tailwind('place-content'(Place)) -->
    "place-content-", one_of(["start", "end", "center", "between",
                             "around", "evenly", "stretch"], P), !,
    { place_content_place(P, Place) }.

tailwind('place-items'(P)) -->
    "place-items-", one_of(["auto", "start", "end", "center", "stretch"], P).

tailwind('place-self'(P)) -->
    "place-self-", one_of(["auto", "start", "end", "center", "stretch"], P).

% effect

shadow_size_style(sm, "0 1px 2px 0 rgba(0,0,0,0.05)").
shadow_size_style(md, "0 4px 6px -1px rgba(0,0,0,0.1),0 2px 4px -1px rgba(0,0,0,0.06)").
shadow_size_style(lg, "0 10px 15px -3px rgba(0,0,0,0.1),0 4px 6px -2px rgba(0,0,0,0.05)").
shadow_size_style(xl, "0 20px 25px -5px rgba(0,0,0,0.1),0 10px 10px -5px rgba(0,0,0,0.04)").
shadow_size_style('2xl', "0 25px 50px -12px rgba(0,0,0,0.25)").
shadow_size_style(inner, "inset 0 2px 4px 0 rgba(0,0,0,0.06)").
shadow_size_style(none, "0 0 #0000").

tailwind(['--pl-shadow'(Shadow),
          'box-shadow'("var(--pl-ring-offset-shadow,0 0 #0000),var(--pl-ring-shadow,0 0 #0000),var(--pl-shadow)")]) -->
    "shadow-",
    one_of(["sm", "md", "lg", "xl", "2xl", "inner", "none"],
           Val), !,
    { shadow_size_style(Val, Shadow) }.
tailwind(['--pl-shadow'("0 1px 3px 0 rgba(0,0,0,0.1),0 1px 2px 0 rgba(0,0,0,0.06)"),
          'box-shadow'("var(--pl-ring-offset-shadow,0 0 #0000),var(--pl-ring-shadow,0 0 #0000),var(--pl-shadow)")]) -->
    "shadow", !.

tailwind(opacity(Opacity)) -->
    "opacity-", num(Num), !,
    { value_unit_css(Num, Opacity, _{value_fn: div_100}) }.

% grid

tailwind('grid-template-columns'(none)) --> "grid-cols-none", !.
tailwind('grid-template-columns'(none)) --> "grid-cols-0", !.
tailwind('grid-template-columns'(Style)) -->
    "grid-cols-", integer(Int), !,
    { format(string(Style), "repeat(~d, minmax(0, 1fr))", [Int]) }.

tailwind('grid-column'(auto)) --> "col-auto", !.

tailwind('grid-column'("-1 / 1")) --> "col-span-full", !.
tailwind('grid-column'(Span)) -->
    "col-span-", integer(Int), !,
    { format(string(Span), "span ~d / span ~d", [Int, Int]) }.

tailwind('grid-column-start'("auto")) --> "col-start-auto", !.
tailwind('grid-column-start'(Start)) -->
    "col-start-", integer(Start), !.

tailwind('grid-template-rows'("none")) --> "grid-rows-none", !.
tailwind('grid-template-rows'("none")) --> "grid-rows-0", !.
tailwind('grid-template-rows'(Rows)) -->
    "grid-rows-", integer(Int), !,
    { format(string(Rows), "repeat(~d, minmax(0, 1fr))", [Int]) }.

tailwind('grid-row'("auto")) --> "row-auto", !.

tailwind('grid-row'("-1 / 1")) --> "row-span-full", !.
tailwind('grid-row'(Span)) -->
    "row-span-", integer(Int), !,
    { format(string(Span), "span ~d / span ~d", [Int, Int]) }.

tailwind('grid-row-start'("auto")) --> "row-start-auto", !.
tailwind('grid-row-start'(Start)) --> "row-start-", integer(Start), !.

grid_flow_style(row, "row").
grid_flow_style(col, "col").
grid_flow_style('row-dense', "row dense").
grid_flow_style('col-dense', "col dense").

tailwind('grid-auto-flow'(Flow)) -->
    "grid-flow-", one_of(["row-dense", "col-dense", "row", "col"],
                         FlowType), !,
    { grid_flow_style(FlowType, Flow) }.

grid_auto_style(auto, "auto").
grid_auto_style(min, "min-content").
grid_auto_style(max, "max-content").
grid_auto_style(fr, "minmax(0, 1fr)").

tailwind('grid-auto-columns'(Cols)) -->
    "auto-cols-", one_of(["auto", "min", "max", "fr"],
                        AutoType), !,
    { grid_auto_style(AutoType, Cols) }.

tailwind('grid-auto-rows'(Rows)) -->
    "auto-rows-", one_of(["auto", "min", "max", "fr"],
                         AutoType), !,
    { grid_auto_style(AutoType, Rows) }.

gap_type(Val) -->
    alternates([percentage(V), length(V), length_unit(V), num(V)]),
    { value_unit_css(V, Val, _{zero_unit: "", number: _{unit: rem,
                                                        value_fn: div_4}}) }.

tailwind('column-gap'(Val)) -->
    "gap-x-", gap_type(Val), !.
tailwind('row-gap'(Val)) -->
    "gap-y-", gap_type(Val), !.
tailwind('gap'(Val)) -->
    "gap-", gap_type(Val), !.

% interactivity

tailwind(appearance(none)) --> "appearance-none", !.

tailwind(cursor(Type)) -->
    "cursor-", one_of(["auto", "default", "pointer", "wait", "text", "move",
                       "not-allowed"], Type), !.

outline_type(none, "2px solid transparent").
outline_type(white, "2px dotted white").
outline_type(black, "2px dotted black").

tailwind(['outline-offset'("2px"), outline(Type)]) -->
    "outline-", one_of(["none", "white", "black"], Type_), !,
    { outline_type(Type_, Type) }.

tailwind('pointer-events'(Events)) -->
    "pointer-events-", one_of(["none", "auto"], Events), !.

resize_type(none, "none").
resize_type(x, "horizontal").
resize_type(y, "vertical").

tailwind(resize(Resize)) -->
    "resize-", one_of(["none", "x", "y"], Type), !,
    { resize_type(Type, Resize) }.
tailwind(resize("both")) --> "resize", !.

tailwind('user-select'(Type)) -->
    "select-", one_of(["none", "text", "all", "auto"], Type), !.

% layout

% [XXX]: if there's a media query min-width, then this should be
% {:max-width (breakpoint->pixels media-query-min-width)}
% but...right now don't have that exposed.
tailwind(width("100%")) --> "container", !.

tailwind('box-sizing'("border-box")) --> "box-border", !.
tailwind('box-sizing'("content-box")) --> "box-content", !.

tailwind(display("none")) --> "hidden", !.
tailwind(display(Display)) -->
    one_of(["block", "inline-block", "flex", "inline-flex",
            "inline-grid", "grid", "table-column-group",
            "table-footer-group", "table-header-group",
            "table-row-group",
            "table-column", "table-cell", "table-row", "table-caption",
            "table", "contents", "hidden", "flow-root"
           ], Display), !.

tailwind(float(Dir)) --> "float-", one_of(["left", "right", "none"], Dir), !.

tailwind(clear(Dir)) -->
    "clear-", one_of(["left", "right", "both", "none"], Dir), !.

tailwind('object-fit'(Fit)) -->
    "object-", one_of(["contain", "cover", "fill", "none", "scale-down"], Fit),
    !.

tailwind(Style) -->
    "overflow-", axis(axis(Axis)), "-",
    one_of(["auto", "hidden", "visible", "scroll"], Mode), !,
    { format(atom(Prop), "overflow-~w", [Axis]),
      Style =.. [Prop, Mode] }.
tailwind(overflow(Mode)) -->
    "overflow-", one_of(["auto", "hidden", "visible", "scroll"], Mode), !.

tailwind(Style) -->
    "overscroll-", axis(axis(Axis)), "-",
    one_of(["auto", "contain", "none"], Mode), !,
    { format(atom(Prop), "overscroll-~w", [Axis]),
      Style =.. [Prop, Mode] }.
tailwind(overscroll(Mode)) -->
    "overscroll-", one_of(["auto", "contain", "none"], Mode), !.

tailwind(position(Pos)) -->
    one_of(["static", "fixed", "absolute", "relative", "sticky"], Pos), !.

tailwind(Styles) -->
    soft_optional(signus(S), { S = '+' }),
    one_of(["top", "right", "bottom", "left",
            "inset-x", "inset-y", "inset"], Mode),
    "-",
    alternates([length(Val), length_unit(Val), fraction(Val),
               percentage(Val), full_100(Val), auto(Val),
               num(Val)]), !,
    { value_unit_css(Val, Value, _{signus: S,
                                   zero_unit: "",
                                   number: _{unit: rem, value_fn: div_4},
                                   fraction: _{unit: "%", value_fn: mul_100}}),
      positioning_mode_dirs(Mode, Dirs),
      maplist({Value}/[Dir, Style]>>(Style =.. [Dir, Value]),
             Dirs, Styles) }.

positioning_mode_dirs(inset, [top, right, bottom, left]) :- !.
positioning_mode_dirs('inset-x', [right, left]) :- !.
positioning_mode_dirs('inset-y', [top, bottom]) :- !.
positioning_mode_dirs(Dir, [Dir]).

tailwind(visibility(Vis)) --> one_of(["visibile", "invisible"], Vis), !.

tailwind('z-index'("auto")) --> "z-auto", !.
tailwind('z-index'(Index)) --> "z-", integer(Index), !.

% Sizing

tailwind(width(Width)) -->
    "w-", alternates([length(Val), length_unit(Val), fraction(Val),
                      percentage(Val), full_100(Val), auto(Val),
                      screen_100vw(Val), min_content(Val), max_content(Val),
                      num(Val)]), !,
    { value_unit_css(Val, Width, _{zero_unit: "",
                                   number: _{unit: rem, value_fn: div_4},
                                   fraction: _{unit: '%', value_fn: mul_100}}) }.

tailwind('min-width'(Width)) -->
    "min-w-", alternates([length(Val), length_unit(Val), fraction(Val),
                      percentage(Val), full_100(Val), auto(Val),
                      screen_100vw(Val), min_content(Val), max_content(Val),
                      num(Val)]), !,
    { value_unit_css(Val, Width, _{zero_unit: "",
                                   number: _{unit: rem, value_fn: div_4},
                                   fraction: _{unit: '%', value_fn: mul_100}}) }.

tailwind('max-width'(Width)) -->
    "max-w-", alternates([length(Val), length_unit(Val), fraction(Val),
                      percentage(Val), full_100(Val), auto(Val),
                      screen_100vw(Val), min_content(Val), max_content(Val),
                      num(Val)]), !,
    { value_unit_css(Val, Width, _{zero_unit: "",
                                   number: _{unit: rem, value_fn: div_4},
                                   fraction: _{unit: '%', value_fn: mul_100}}) }.

tailwind(height(Height)) -->
    "h-", alternates([length(Val), length_unit(Val), fraction(Val),
                      percentage(Val), full_100(Val), auto(Val),
                      screen_100vw(Val), min_content(Val), max_content(Val),
                      num(Val)]), !,
    { value_unit_css(Val, Height, _{zero_unit: "",
                                   number: _{unit: rem, value_fn: div_4},
                                   fraction: _{unit: '%', value_fn: mul_100}}) }.

tailwind('min-height'(Height)) -->
    "min-h-", alternates([length(Val), length_unit(Val), fraction(Val),
                      percentage(Val), full_100(Val), auto(Val),
                      screen_100vw(Val), min_content(Val), max_content(Val),
                      num(Val)]), !,
    { value_unit_css(Val, Height, _{zero_unit: "",
                                   number: _{unit: rem, value_fn: div_4},
                                   fraction: _{unit: '%', value_fn: mul_100}}) }.

tailwind('max-height'(Height)) -->
    "max-h-", alternates([length(Val), length_unit(Val), fraction(Val),
                      percentage(Val), full_100(Val), auto(Val),
                      screen_100vw(Val), min_content(Val), max_content(Val),
                      num(Val)]), !,
    { value_unit_css(Val, Height, _{zero_unit: "",
                                   number: _{unit: rem, value_fn: div_4},
                                   fraction: _{unit: '%', value_fn: mul_100}}) }.

% spacing

dir_axis_props(axis(x), [left, right]).
dir_axis_props(axis(y), [top, bottom]).
dir_axis_props(direction(t), [top]).
dir_axis_props(direction(r), [right]).
dir_axis_props(direction(b), [bottom]).
dir_axis_props(direction(l), [left]).

tailwind(padding(Padding)) -->
    soft_optional(signus(S), { S = '+' }),
    "p-", alternates([length(Val), length_unit(Val), num(Val)]), !,
    { value_unit_css(Val, Padding, _{signus: S, zero_unit: "",
                                    number: _{unit: rem, value_fn: div_4}}) }.
tailwind(Styles) -->
    soft_optional(signus(S), { S = '+' }),
    "p", alternates([axis(A), direction(A)]), "-",
    alternates([length(Val), length_unit(Val), num(Val)]), !,
    { value_unit_css(Val, Padding, _{signus: S, zero_unit: "",
                                    number: _{unit: rem, value_fn: div_4}}),
      dir_axis_props(A, Props),
      maplist({Padding}/[Prop, Style]>>(
                  format(atom(Attr), "padding-~w", [Prop]),
                  Style =.. [Attr, Padding]),
             Props, Styles) }.

tailwind(margin(Margin)) -->
    soft_optional(signus(S), { S = '+' }),
    "m-", alternates([length(Val), length_unit(Val), num(Val)]), !,
    { value_unit_css(Val, Margin, _{signus: S, zero_unit: "",
                                    number: _{unit: rem, value_fn: div_4}}) }.
tailwind(Styles) -->
    soft_optional(signus(S), { S = '+' }),
    "m", alternates([axis(A), direction(A)]), "-",
    alternates([length(Val), length_unit(Val), num(Val)]), !,
    { value_unit_css(Val, Margin, _{signus: S, zero_unit: "",
                                    number: _{unit: rem, value_fn: div_4}}),
      dir_axis_props(A, Props),
      maplist({Margin}/[Prop, Style]>>(
                  format(atom(Attr), "margin-~w", [Prop]),
                  Style =.. [Attr, Margin]),
             Props, Styles) }.

space_between_reverse(true) --> "-reverse".
space_between_reverse(false) --> [].

space_between_prop(x, false, left).
space_between_prop(x, true, right).
space_between_prop(y, false, top).
space_between_prop(y, true, bottom).

tailwind('&'('> * + *'(Style))) -->
    soft_optional(signus(S), { S = '+' }),
    "space-", axis(axis(A)), "-",
    alternates([length(Val), length_unit(Val), num(Val)]),
    space_between_reverse(Rev), !,
    { space_between_prop(A, Rev, Dir), !,
      format(atom(Prop), "margin-~w", [Dir]),
      value_unit_css(Val, Value, _{signus: S, zero_unit: "",
                                   number: _{unit: rem, value_fn: div_4}}),
      Style =.. [Prop, Value] }.

% svg

tailwind(fill(ColourCss)) -->
    "fill-", colour(Colour), !,
    { colour_css(Colour, ColourCss) }.

tailwind(stroke(ColourCss)) -->
    "stroke-", colour(Colour), !,
    { colour_css(Colour, ColourCss) }.

tailwind('stroke-width'(Thickness)) -->
    "stroke-", num(number(Thickness)), !.

% table

tailwind('border-collapse'(Type)) -->
    "border-", one_of(["collapse", "separate"], Type), !.

tailwind('table-layout'(Type)) -->
    "table-", one_of(["auto", "fixed"], Type), !.

% transform

tailwind(transform(none)) --> "transform-none", !.
tailwind(['--pl-translate-x'(0),
         '--pl-translate-y'(0),
         '--pl-rotate'(0),
         '--pl-skew-x'(0),
         '--pl-skew-y'(0),
         '--pl-scale-x'(0),
         '--pl-scale-y'(0),
         transform("translate3d(var(--pl-translate-x),var(--pl-translate-y),0) rotate(var(--pl-rotate)) skewX(var(--pl-skew-x)) skewY(var(--pl-skew-y)) scaleX(var(--pl-scale-x)) scaleY(var(--pl-scale-y))")
         ]) -->
    "transform-gpu", !.
tailwind(['--pl-translate-x'(0),
         '--pl-translate-y'(0),
         '--pl-rotate'(0),
         '--pl-skew-x'(0),
         '--pl-skew-y'(0),
         '--pl-scale-x'(0),
         '--pl-scale-y'(0),
         transform("translateX(var(--pl-translate-x)) translateY(var(--pl-translate-y)) rotate(var(--pl-rotate)) skewX(var(--pl-skew-x)) skewY(var(--pl-skew-y)) scaleX(var(--pl-scale-x)) scaleY(var(--pl-scale-y))")
         ]) -->
    "transform", !.

transform_origin_pos('top-left', "top left") :- !.
transform_origin_pos('top-right', "top right") :- !.
transform_origin_pos('bottom-left', "bottom left") :- !.
transform_origin_pos('bottom-right', "bottom right") :- !.
transform_origin_pos(P, P).

tailwind('transform-origin'(Origin)) -->
    "origin-", one_of(["top-left", "top-right", "top",
                      "bottom-left", "bottom-right", "bottom",
                      "left", "center", "right"],
                     Orig), !,
    { transform_origin_pos(Orig, Origin) }.

tailwind(Style) -->
    soft_optional(signus(S), { S = '+' }),
    "scale-", axis(axis(A)), "-", num(Val), !,
    { value_unit_css(Val, CssVal, _{signus: S, value_fn: div_100}),
      format(atom(Prop), "--pl-scale-~w", [A]),
      Style =.. [Prop, CssVal] }.
tailwind(['--pl-scale-x'(CssVal), '--pl-scale-y'(CssVal)]) -->
    soft_optional(signus(S), { S = '+' }),
    "scale-", num(Val), !,
    { value_unit_css(Val, CssVal, _{signus: S, value_fn: div_100}) }.

tailwind('--pl-rotate'(Rotate)) -->
    soft_optional(signus(S), { S = '+' }),
    "rotate-", alternates([angle(V), num(V)]), !,
    { value_unit_css(V, Rotate, _{signus: S, zero_unit: "",
                                  number: _{unit: deg}}) }.

tailwind(Style) -->
    soft_optional(signus(S), { S = '+' }),
    "translate-", axis(axis(A)), "-",
    alternates([length(V), length_unit(V), fraction(V),
                percentage(V), full_100(V), num(V)]), !,
    { format(atom(Prop), "--pl-translate-~w", [A]),
      value_unit_css(V, Val, _{signus: S, zero_unit: "",
                               number: _{unit: rem, value_fn: div_4},
                               fraction: _{unit: "%", value_fn: mul_100}}),
      Style =.. [Prop, Val] }.

tailwind(Style) -->
    soft_optional(signus(S), { S = '+' }),
    "skew-", axis(axis(A)), "-", alternates([angle(V), num(V)]),
    { value_unit_css(V, Skew, _{signus: S, zero_unit: "",
                                number: _{unit: deg}}),
      format(atom(Prop), "--pl-skew-~w", [A]),
      Style =.. [Prop, Skew] }.

% typography

default_font_family(sans, "ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"").
default_font_family(serif, "ui-serif, Georgia, Cambria, \"Times New Roman\", Times, serif").
default_font_family(mono, "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace").

tailwind('font-family'(FontFamily)) -->
    "font-", one_of(["sans", "serif", "mono"], Family), !,
    { default_font_family(Family, FontFamily) }.

tailwind(['font-size'(SizeRem), 'line-height'(HeightRem)]) -->
    "text-", one_of(["xs", "sm", "base", "lg", "xl", "2xl", "3xl",
                     "4xl", "5xl", "6xl", "7xl", "8xl", "9xl"],
                   SzCls), !,
    { font_class_size_height(SzCls, Size, Height),
      format(string(SizeRem), "~wrem", [Size]),
      format(string(HeightRem), "~wrem", [Height]) }.

font_class_size_height(xs, 0.75, 1).
font_class_size_height(sm, 0.875, 1.25).
font_class_size_height(base, 1, 1.5).
font_class_size_height(lg, 1.125, 1.75).
font_class_size_height(xl, 1.25, 1.75).
font_class_size_height('2xl', 1.5, 2).
font_class_size_height('3xl', 1.875, 2.25).
font_class_size_height('4xl', 2.25, 2.5).
font_class_size_height('5xl', 3, 1).
font_class_size_height('6xl', 3.75, 1).
font_class_size_height('7xl', 4.5, 1).
font_class_size_height('8xl', 6, 1).
font_class_size_height('9xl', 8, 1).

% non-tailwind extension, giroutte-style
tailwind('font-size'(Size)) -->
    "font-size-", alternates([length(V), fraction(V), percentage(V), num(V)]), !,
    { value_unit_css(V, Size, _{number: _{unit: rem, value_fn: div_4},
                                fraction: _{unit: '%', value_fn: mul_100}}) }.

tailwind(['-webkit-font-smoothing'("antialiased"),
         '-moz-osx-font-smoothing'("grayscale")]) -->
    "antialiased", !.
tailwind(['-webkit-font-smoothing'("auto"),
          '-moz-osx-font-smoothing'("auto")]) -->
    "subpixel-antialiased", !.

tailwind('font-style'("italic")) --> "italic", !.
tailwind('font-style'("normal")) --> "not-italic", !.

tailwind('font-weight'(Weight)) -->
    "font-", one_of(["thin", "extralight", "light", "normal",
                     "medium", "semibold", "bold", "extrabold", "black"],
                    WeightType), !,
    { font_type_weight(WeightType, Weight) }.

font_type_weight(thin, 100).
font_type_weight(extralight, 200).
font_type_weight(light, 300).
font_type_weight(normal, 400).
font_type_weight(medium, 500).
font_type_weight(semibold, 600).
font_type_weight(bold, 700).
font_type_weight(extrabold, 800).
font_type_weight(black, 900).

tailwind('font-variant-numeric'(Variant)) -->
    one_of(["normal-nums", "ordinal", "slashed-zero",
           "lining-nums", "oldstyle-nums", "proportional-nums",
           "tabular-nums", "diagonal-fractions", "stacked-fractions"],
          Variant), !.

tailwind('letter-spacing'(SpacingEm)) -->
    "tracking-", one_of(["tighter", "tight", "normal",
                         "wider", "widest", "wide"], Size), !,
    { letter_spacing(Size, Spacing),
      format(string(SpacingEm), "~wem", [Spacing]) }.

letter_spacing(tighter, -0.05).
letter_spacing(tight, -0.025).
letter_spacing(normal, 0).
letter_spacing(wide, 0.025).
letter_spacing(wider, 0.05).
letter_spacing(widest, 0.1).

tailwind('line-height'(LineHeight)) -->
    "leading-", one_of(["none", "tight", "snug", "normal",
                        "relaxed", "loose"], SizeName), !,
    { line_height_size(SizeName, LineHeight) }.
tailwind('line-height'(LineHeight)) -->
    "leading-", num(Size), !,
    { value_unit_css(Size, LineHeight, _{zero_unit: "",
                                        number: _{unit: rem,
                                                  value_fn: div_4}}) }.


line_height_size(none, 1).
line_height_size(tight, 1.25).
line_height_size(snug, 1.375).
line_height_size(normal, 1.5).
line_height_size(relaxed, 1.625).
line_height_size(loose, 2).

% non-tailwind extension, giroutte-style
tailwind('line-height'(Height)) -->
    "line-height-",
    alternates([length(V), fraction(V), percentage(V), num(V)]), !,
    { value_unit_css(V, Height, _{fraction: _{unit: '%', value_fn: mul_100}}) }.

tailwind('list-style-type'(Type)) -->
    "list-", one_of(["none", "disc", "decimal"], Type), !.

tailwind('list-style-position'(Type)) -->
    "list-", one_of(["inside", "outside"], Type), !.

tailwind('&'('::placeholder'(Style))) -->
    "placeholder-", colour(Colour), !,
    { colour_placeholder(Colour, Style) }.

colour_placeholder(special(S), S) :- !.
colour_placeholder(Colour, CssColour) :-
    ( has_alpha(Colour)
    -> Colour_ = Colour
    ;  colour_with_alpha(Colour, "var(--pl-placeholder-opacity,1)", Colour_) ),
    colour_css(Colour_, CssColour).

tailwind('--pl-placeholder-opacity'(Opacity)) -->
    "placeholder-opacity-", num(Val), !,
    { value_unit_css(Val, Opacity, _{value_fn: div_100}) }.

tailwind('text-align'(Align)) -->
    "text-", one_of(["left", "center", "right", "justify"], Align), !.

tailwind(color(ColourCss)) -->
    "text-", colour(Colour), !,
    { colour_text(Colour, ColourCss) }.

colour_text(special(S), S) :- !.
colour_text(Colour, CssColour) :-
    ( has_alpha(Colour)
    -> Colour_ = Colour
    ;  colour_with_alpha(Colour, "var(--pl-text-opacity,1)", Colour_) ),
    colour_css(Colour_, CssColour).

tailwind('--pl-text-opacity'(Opacity)) -->
    "text-opacity-", num(Num), !,
    { value_unit_css(Num, Opacity, _{value_fn: div_100}) }.

tailwind('text-decoration'("none")) --> "no-underline", !.
tailwind('text-decoration'(Decoration)) -->
    one_of(["underline", "line-through"], Decoration), !.

tailwind('text-transform'("none")) --> "normal-case", !.
tailwind('text-transform'(Trans)) -->
    one_of(["uppercase", "lowercase", "capitalize"], Trans), !.

tailwind([overflow(hidden),
          'text-overflow'(ellipsis),
          'white-space'(nowrap)]) -->
    "truncate", !.
tailwind('text-overflow'(ellipsis)) --> "overflow-ellipsis", !.
tailwind('text-overflow'(clip)) --> "overflow-clip", !.

tailwind('vertical-align'(Align)) -->
    "align-", one_of(["baseline", "top", "middle", "bottom",
                      "text-top", "text-bottom"], Align), !.

tailwind('white-space'(Control)) -->
    "whitespace-", one_of(["normal", "nowrap", "pre-line", "pre-wrap",
                          "pre"], Control), !.

tailwind(['overflow-wrap'(normal), 'word-break'(normal)]) -->
    "break-normal", !.
tailwind('overflow-wrap'("break-word")) --> "break-words", !.
tailwind('word-break'("break-all")) --> "break-all", !.
