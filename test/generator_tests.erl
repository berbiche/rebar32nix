-module(generator_tests).

-include_lib("eunit/include/eunit.hrl").

generation_test() ->
    FileName = filename:absname("test/tmp-deps.term"),
    {ok, [Deps]} = file:consult(FileName),
    App = #{
            name => toto,
            vsn => "git",
            src => "./",
            deps => Deps,
            release_type => release,
            builder => "buildErlangMk"
           },
    Res = generator:new(App),
    Doc = prettypr:format(Res),
    ?debugFmt("~s~n", [Doc]),
    ok.