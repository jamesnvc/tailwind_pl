:- module(tw_reset, [reset_style/1]).

:- use_module(library(css_write), [write_css/2, css//1]).

% adapted from https://github.com/green-coder/girouette/blob/master/lib/girouette/src/girouette/tw/preflight.cljc
% tailwindcss v2.0.3 | MIT License | https://tailwindcss.com
% modern-normalize v1.0.0 | MIT License | https://github.com/sindresorhus/modern-normalize
reset_style(Style) :-
    write_css(
        css(['*, ::before, ::after'('box-sizing'("border-box")),

             '.root'(['-moz-tab-size'(4), 'tab-size'(4)]),

             'html'('-webkit-text-size-adjust'("100%")),

             'body'(margin(0)),

             'body'('font-family'("system-ui, apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji'")),

             hr([height(0), color(inherit), 'border-top-width'("1px")]),

             'abbr, [title]'(['-webkit-text-decoration'("underline dotted"),
                            'text-decoration'("underline dotted")]),

             'b, strong'('font-weight'(bolder)),

             'code, kbd, samp, pre'(['font-family'("ui-monospace, SFMono-Regular, Consolas, 'Liberation Mono', Menlo, monospace"),
                                     'font-size'("1em")]),

             small('font-size'("80%")),

             'sub, sup'(['font-size'("75%"),
                         'line-height'(0),
                         position(relative),
                         'vertical-align'(baseline)]),
             sub(bottom("-0.25em")),
             sup(top("-0.5em")),

             table(['text-indent'(0), 'border-color'(inherit)]),

             'button, input, optgroup, select, textarea'(
                 ['font-family'(inherit),
                  'font-size'("100%"),
                  'line-height'("1.15"),
                  'margin'(0)
                 ]),

             'button, select'('text-transform'(none)),

             'button, [type="button"], [type="reset"], [type="submit"]'(
                 '-webkit-appearance'(button)
             ),

             '::-moz-focus-inner'(['border-style'(none), padding(0)]),

             ':-moz-focusring'(outline("1px dotted ButtonText")),

             ':-moz-ui-invalid'('box-shadow'(none)),

             legend(padding(0)),

             progress('vertical-align'(baseline)),

             '::-webkit-inner-spin-button, ::-webkit-outer-spin-button'(height(auto)),

             '[type="search"]'(['-webkit-appearance'(textfield),
                                'outline-offset'("-2px")]),

             '::-webkit-file-upload-button'(['-webkit-appearance'(button),
                                             font(inherit)]),

             summary(display("list-item")),

             'blockquote, dl, dd, h1, h2, h3, h4, h5, h6, hr, figure, p, pre'(
                 margin(0)
             ),

             button(['background-color'(transparent), 'background-image'(none)]),

             'button:focus'([outline("1px dotted"),
                             outline("5px auto -webkit-focus-ring-color")]),

             fieldset([margin(0), padding(0)]),

             'ol, ul'(['list-style'(none), margin(0), padding(0)]),

             % another font change & line-height?

             html(['font-family'(inherit), 'line-height'(inherit)]),

             '*, ::before, ::after'(['border-width'(0),
                                     'border-style'(solid),
                                     'border-color'("#e5e7eb")]),

             hr('border-top-width'("1px")),

             textarea(resize(vertical)),

             'input::placeholder, textarea::placeholder'(
                 [opacity(1), color("#9ca3af")]
             ),

             'button, [role="button"]'(cursor(pointer)),

             table('border-collapse'(collapse)),

             'h1,h2,h3,h4,h5,h6'(['font-size'(inherit), 'font-weight'(inherit)]),

             a([color(inherit), 'text-decoration'(inherit)]),

             'button, input, optgroup, select, textarea'(
                 [padding(0), 'line-height'(inherit), color(inherit)]
             ),

             'img, video'(['max-width'("100%"), height(auto)])

            ]),
        Style
    ).
