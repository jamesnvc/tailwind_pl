:- module(grammar_t, []).

:- use_module(library(plunit)).
:- use_module(tailwind/grammar, [tailwind//1]).
:- begin_tests(tailwind).

test(prefixes_1,
     [ true(Medias-States == [
                color_scheme(light), min_width("640px")
            ]-[inner(hover), inner(checked)]) ]) :-
    phrase(grammar:prefixes(Medias, States), `light:sm:hover:checked:`).
test(prefixes_2,
     [ true(Medias-States == [
                color_scheme(light), min_width("640px")
            ]-[inner('first-of-type'), outer('.group:disabled'), inner("first-child")]) ]) :-
    phrase(grammar:prefixes(Medias, States),
           `light:sm:first-of-type:group-disabled:first:`).

test(flex_shorthand_1,
    [ true(Flex == flex("5 5 0%")) ]) :-
    phrase(tailwind(Flex),
           `flex-5`).
test(flex_shorthand_2,
    [ true(Flex == flex("0.5 0.5 0%")) ]) :-
    phrase(tailwind(Flex),
           `flex-1/2`).
test(flex_shorthand_3,
    [ true(Flex == flex("0 1 auto")) ]) :-
    phrase(tailwind(Flex),
           `flex-initial`).

test(flex_basis_1,
    [ true(Basis == 'flex-basis'(1)) ]) :-
    phrase(tailwind(Basis), `flex-basis`).
test(flex_basis_2,
    [ true(Basis == 'flex-basis'("3rem")) ]) :-
    phrase(tailwind(Basis), `flex-basis-12`).
test(flex_basis_3,
    [ true(Basis == 'flex-basis'("75.0%")) ]) :-
    phrase(tailwind(Basis), `flex-basis-3/4`).
test(flex_basis_4,
    [ true(Basis == 'flex-basis'("1lh")) ]) :-
    phrase(tailwind(Basis), `flex-basis-lh`).

test(flex_1,
    [ true(Flex == flex("0 1 auto"))]) :-
    phrase(tailwind(Flex), `flex-initial`).
test(flex_2,
    [ true(Flex == flex("17 17 0%"))]) :-
    phrase(tailwind(Flex), `flex-17`).
test(flex_3,
    [ true(Flex == flex("17 0.75"))]) :-
    phrase(tailwind(Flex), `flex-17-3/4`).
test(flex_4,
    [ true(Flex == flex("17 100%"))]) :-
    phrase(tailwind(Flex), `flex-17-full`).
test(flex_5,
    [ true(Flex == flex("17 0.75 5rem"))]) :-
    phrase(tailwind(Flex), `flex-17-3/4-20`).

test(order_1, [ true(O == order(-9999)) ]) :-
    phrase(tailwind(O), `order-first`).
test(order_2, [ true(O == order("17")) ]) :-
    phrase(tailwind(O), `order-17`).
test(order_3, [ true(O == order("17")) ]) :-
    phrase(tailwind(O), `+order-17`).
test(order_4, [ true(O == order("-17")) ]) :-
    phrase(tailwind(O), `-order-17`).

test(duration_1,
    [ true(Dur == 'transition-duration'("15ms"))]) :-
    phrase(tailwind(Dur), `duration-15`).
test(duration_2,
    [ true(Dur == 'transition-duration'("1s"))]) :-
    phrase(tailwind(Dur), `duration-1s`).
test(duration_3,
    [ true(Dur == 'transition-duration'("0s"))]) :-
    phrase(tailwind(Dur), `duration-0`).

test(bg_color_1,
    [ true(Bg == 'background-color'("#f9a8d40c")) ]) :-
    phrase(tailwind(Bg), `bg-pink-300-5`).
test(bg_color_2,
    [ true(Bg == 'background-color'("#c0ffee00")) ]) :-
    phrase(tailwind(Bg), `bg-rgba-c0ffee00`).
test(bg_color_3,
    [ true(Bg == 'background-color'("#c0ffee01")) ]) :-
    phrase(tailwind(Bg), `bg-#c0ffee01`).
test(bg_color_4,
    [ true(Bg == 'background-color'("rgba(249, 168, 212, var(--pl-bg-opacity, 1))")) ]) :-
    phrase(tailwind(Bg), `bg-pink-300`).
test(bg_color_5,
    [ true(Bg == 'background-color'("rgba(168, 85, 247, var(--pl-bg-opacity, 1))")) ]) :-
    phrase(tailwind(Bg), `bg-purple-500`).

test(bg_size_1,
    [ true(Bg == 'background-size'(cover)) ]) :-
    phrase(tailwind(Bg), `bg-cover`).
test(bg_size_2,
    [ true(Bg == 'background-size'("1cm 75.0%")) ]) :-
    phrase(tailwind(Bg), `bg-size-1cm-3/4`).
test(bg_size_2,
    [ true(Bg == 'background-size'("3rem auto")) ]) :-
    phrase(tailwind(Bg), `bg-size-12-auto`).
test(bg_size_2,
    [ true(Bg == 'background-size'("3rem 1px")) ]) :-
    phrase(tailwind(Bg), `bg-size-12-px`).


test(bg_img_1,
    [ true(Bg == 'background-image'("none")) ]) :-
    phrase(tailwind(Bg), `bg-none`).
test(bg_img_2,
    [ true(Bg == 'background-image'("linear-gradient(to bottom left, var(--pl-gradient-stops))")) ]) :-
    phrase(tailwind(Bg), `bg-gradient-to-bl`).

test(bg_gradient_1,
    [ true(Grad == ['--pl-gradient-from'("#f1f5f9"),
                    '--pl-gradient-stops'("var(--pl-gradient-from), var(--pl-gradient-to, #f1f5f900)")
                   ]) ]) :-
    phrase(tailwind(Grad), `from-blueGray-100`).

test(bg_opacity_1,
    [ true( Style == '--pl-bg-opacity'("0.305")) ]) :-
    phrase(tailwind(Style), `bg-opacity-30.5`).

test(border_radius_1,
    [ true(Rad == 'border-radius'("0.25rem")) ]) :-
    phrase(tailwind(Rad), `rounded`).
test(border_radius_2,
    [ true(Rad == 'border-radius'("1.0rem")) ]) :-
    phrase(tailwind(Rad), `rounded-2xl`).
test(border_radius_3,
    [ true(Rad == 'border-radius'("0px")) ]) :-
    phrase(tailwind(Rad), `rounded-none`).
test(border_radius_4,
    [ true(Rad == ['border-bottom-right-radius'("0.5rem"),
                   'border-bottom-left-radius'("0.5rem")]) ]) :-
    phrase(tailwind(Rad), `rounded-b-lg`).
test(border_radius_5,
    [ true(Rad == ['border-bottom-right-radius'("0.25rem"),
                   'border-bottom-left-radius'("0.25rem")]) ]) :-
    phrase(tailwind(Rad), `rounded-b`).

test(border_width_1,
    [ true(Style == 'border-width'("1px")) ]) :-
    phrase(tailwind(Style), `border`).
test(border_width_2,
    [ true(Style == 'border-right-width'("1px")) ]) :-
    phrase(tailwind(Style), `border-r`).
test(border_width_3,
    [ true(Style == 'border-width'("1rem")) ]) :-
    phrase(tailwind(Style), `border-rem`).
test(border_width_4,
    [ true(Style == 'border-width'("12pt")) ]) :-
    phrase(tailwind(Style), `border-12pt`).
test(border_width_5,
    [ true(Style == 'border-bottom-width'("12px")) ]) :-
    phrase(tailwind(Style), `border-b-12`).

test(border_color_1,
    [ true(Style == 'border-color'("rgba(192, 255, 238, var(--pl-border-opacity,1))")) ]) :-
    phrase(tailwind(Style), `border-#c0ffee`).

test(divide_width_1,
    [ true(Style == '&'('> * + *'(['border-top-width'("calc(1px * var(--pl-divide-y-reverse))"),
                               'border-bottom-width'("calc(1px * calc(1 - var(--pl-divide-y-reverse)))")]))) ]) :-
    phrase(tailwind(Style), `divide-y`).
test(divide_width_2,
    [ true(Style == '&'('> * + *'(['border-top-width'("calc(13px * var(--pl-divide-y-reverse))"),
                               'border-bottom-width'("calc(13px * calc(1 - var(--pl-divide-y-reverse)))")]))) ]) :-
    phrase(tailwind(Style), `divide-y-13`).
test(divide_width_3,
    [ true(Style == '&'('> * + *'(['border-top-width'("calc(7.5rem * var(--pl-divide-y-reverse))"),
                               'border-bottom-width'("calc(7.5rem * calc(1 - var(--pl-divide-y-reverse)))")]))) ]) :-
    phrase(tailwind(Style), `divide-y-7.5rem`).

test(divide_style_1,
    [ true(Style == '&'('> * + *'('border-style'(dotted)))) ]) :-
    phrase(tailwind(Style), `divide-dotted`).

test(ring_style_1,
    [ true(Style == 'box-shadow'("var(--pl-ring-inset, ) 0 0 0 calc(3px var(--pl-ring-offset-width,0px)) var(--pl-ring-color)")) ]) :-
    phrase(tailwind(Style), `ring`).
test(ring_style_2,
    [ true(Style == 'box-shadow'("var(--pl-ring-inset, ) 0 0 0 calc(7rem + var(--pl-ring-offset-width,0px)) var(--pl-ring-color)")) ]) :-
    phrase(tailwind(Style), `ring-7rem`).
test(ring_style_3,
    [ true(Style == '--pl-ring-inset'("inset")) ]) :-
    phrase(tailwind(Style), `ring-inset`).

test(ring_color_1,
    [ true(Style == '--pl-ring-color'(transparent)) ]) :-
    phrase(tailwind(Style), `ring-transparent`).
test(ring_color_2,
    [ true(Style == '--pl-ring-color'("rgba(202, 254, 186, var(--pl-ring-opacity,1))")) ]) :-
    phrase(tailwind(Style), `ring-#cafeba`).

test(justify_1,
    [ true(Style == 'justify-content'("space-around")) ]) :-
    phrase(tailwind(Style), `justify-around`).

test(gap_1,
    [ true(Style == 'row-gap'("3rem"))]) :-
    phrase(tailwind(Style), `gap-y-12`).
test(gap_2,
    [ true(Style == 'gap'("10%"))]) :-
    phrase(tailwind(Style), `gap-10%`).
test(gap_1,
    [ true(Style == 'gap'("15pt"))]) :-
    phrase(tailwind(Style), `gap-15pt`).

test(positioning_1,
    [ true(Style == [top("3rem"), bottom("3rem")]) ]) :-
    phrase(tailwind(Style), `inset-y-12`).
test(positioning_2,
    [ true(Style == [left("-2px")]) ]) :-
    phrase(tailwind(Style), `-left-2px`).
test(positioning_3,
    [ true(Style == [top("75.0%"), right("75.0%"),
                     bottom("75.0%"), left("75.0%")]) ]) :-
    phrase(tailwind(Style), `+inset-3/4`).

test(padding_1,
     [ true(Style == padding("1rem")) ]) :-
    phrase(tailwind(Style), `p-4`).
test(padding_2,
     [ true(Style == padding("-1px")) ]) :-
    phrase(tailwind(Style), `-p-1px`).
test(padding_3,
    [ true(Style == ['padding-left'("2mm"), 'padding-right'("2mm")] )]) :-
    phrase(tailwind(Style), `+px-2mm`).

test(space_between_1,
     [ true(Style == '&'('> * + *'('margin-top'("3rem")))) ]) :-
    phrase(tailwind(Style), `space-y-12`).
test(space_between_1,
     [ true(Style == '&'('> * + *'('margin-right'("-2pt")))) ]) :-
    phrase(tailwind(Style), `-space-x-2pt-reverse`).

test(rotate_1,
    [ true(Style == '--pl-rotate'("-0.5turn")) ]) :-
    phrase(tailwind(Style), `-rotate-0.5turn`).
test(rotate_2,
    [ true(Style == '--pl-rotate'("90deg")) ]) :-
    phrase(tailwind(Style), `+rotate-90`).

test(text_color_1,
    [ true(Style == color("rgba(255, 255, 255, var(--pl-text-opacity,1))") )]) :-
    phrase(tailwind(Style), `text-white`).

:- end_tests(tailwind).
