:- module(generate_t, []).

:- use_module(library(plunit)).
:- use_module(tailwind/generate).

:- use_module(library(filesex), [relative_file_name/3]).

:- begin_tests(generate).


test(multiple_selectors_1,
    [ true(Css == ['.text-white'(color("rgba(255, 255, 255, var(--pl-text-opacity,1))")),
                   '.bg-purple-500'('background-color'("rgba(168, 85, 247, var(--pl-bg-opacity, 1))")),
                   '.self-start'('align-self'("flex-start")),
                   '.mt-1'(['margin-top'("0.25rem")])
                  ]) ]) :-
    text_tw_css("text-white bg-purple-500 self-start mt-1", Css).

test(multiple_selectors_with_invalid_1,
    [ true(Css == ['.text-white'(color("rgba(255, 255, 255, var(--pl-text-opacity,1))")),
                   '.bg-purple-500'('background-color'("rgba(168, 85, 247, var(--pl-bg-opacity, 1))")),
                   '.self-start'('align-self'("flex-start")),
                   '.mt-1'(['margin-top'("0.25rem")])
                  ]) ]) :-
    text_tw_css("quux text-white bg-purple-500 beep self-start mt-1", Css).


test(variants_1,
    [ true(Css == ['.hover\\:text-rose-700:hover'(color("rgba(190, 18, 60, var(--pl-text-opacity,1))"))]) ]) :-
    text_tw_css('hover:text-rose-700', Css).
test(variants_2,
    [ true(Css == ['.group:hover .group-hover\\:bg-rgba-c0ffeeab'('background-color'("#c0ffeeab"))]) ]) :-
    text_tw_css('group-hover:bg-rgba-c0ffeeab', Css).

test(media_1,
    [ true(Css == ['@media'(and([min_width("768px")]),
                            '.md\\:bg-rgba-c0ffeeab'('background-color'("#c0ffeeab")))]) ]) :-
    text_tw_css('md:bg-rgba-c0ffeeab', Css).
test(media_2,
    [ true(Css == ['@media'(and([min_width("768px")]),
                            '.group:hover .md\\:group-hover\\:bg-rgba-c0ffeeab'('background-color'("#c0ffeeab")))]) ]) :-
    text_tw_css('md:group-hover:bg-rgba-c0ffeeab', Css).

test(text_from_file_1,
     [true(Texts == ["","Back to home","Hello ~w","Increment", "Info","Main Page",
                     "Reloading config","Server started",
                     "Starting remote toplevel on port ~w","Stopping...",
                     "bg-red-50 md:mt-1","here's some stuff, some information",'',
                     '!','%Y-%m-%d %T','../config.pl','/','/counter/inc','/do/thing',
                     '/info','CONFIG_FILE','Hotwire.pl','Port to run API server on',
                     active,api,app,apply_macros,beep,closelog,counter,counter_inc,
                     counter_inc_form,debug,default,form,get,greeting,greeting_form,
                     halt,handler_pool,hello,hidden,home_page,hotwire_script_tag,
                     'hover:bg-pink-100',http,http_dispatch,info_page,int,integer,
                     location,log_topic,main,maybe_start_toplevel,message_context,
                     name,notice,notify,port,post,reload_config,remote_toplevel_port,
                     server,set_cleanup_handlers,shutdown,syslog,term,test_reading,
                     text,'text-red-500',thing,thread,thread_pool,true,use_syslog,
                                        user,usr2])]) :-
    module_property(generate_t, file(ThisFile)),
    relative_file_name(TestFile, ThisFile, "./test_reading.pl"),
    generate:text_from_file(TestFile, Texts).

test(style_from_file_1,
     [true(Style == ['.bg-red-50'('background-color'("rgba(254, 242, 242, var(--pl-bg-opacity, 1))")),
                     '@media'(and([min_width("768px")]),
                              '.md\\:mt-1'(['margin-top'("0.25rem")])),
                     '.hidden'(display("none")),
                     '.hover\\:bg-pink-100:hover'('background-color'("rgba(252, 231, 243, var(--pl-bg-opacity, 1))")),
                     '.text-red-500'(color("rgba(239, 68, 68, var(--pl-text-opacity,1))"))

                    ])]) :-
    module_property(generate_t, file(ThisFile)),
    relative_file_name(TestFile, ThisFile, "./test_reading.pl"),
    tw_from_file(TestFile, Style).
test(style_from_file_2,
     [true(Style == ['.bg-red-50'('background-color'("rgba(254, 242, 242, var(--pl-bg-opacity, 1))")),
                     '@media'(and([min_width("768px")]),
                              '.md\\:mt-1'(['margin-top'("0.25rem")])),
                     '.h-100\\%'(height("100%")),
                     '.hidden'(display("none")),
                     '.hover\\:animate-bounce:hover'([animation("bounce 1s infinite")]),
                     '@keyframes'(bounce,
                                  ['0%, 100%'(
                                       transform("translateY(-25%)"),
                                       'animation-timing-function'("cubic-bezier(0.8,0,1,1)")),
                                   '50%'(transform("translateY(0)"),
                                         'animation-timing-function'("cubic-bezier(0,0,0.2,1)"))]),
                     '.hover\\:bg-pink-100:hover'('background-color'("rgba(252, 231, 243, var(--pl-bg-opacity, 1))")),
                     '.text-red-500'(color("rgba(239, 68, 68, var(--pl-text-opacity,1))"))

                    ])]) :-
    module_property(generate_t, file(ThisFile)),
    relative_file_name(TestFile, ThisFile, "./test_reading_2.pl"),
    tw_from_file(TestFile, Style).

:- end_tests(generate).
