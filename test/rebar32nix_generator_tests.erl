-module(rebar32nix_generator_tests).

-include("rebar32nix_internal.hrl").

-include_lib("eunit/include/eunit.hrl").

generation_test() ->
    FileName = filename:absname("test/tmp-deps.term"),
    {ok, [Deps]} = file:consult(FileName),
    App = #app{
        name = toto,
        vsn = "git",
        % src  = "./.",
        src = #gitDep{name = "", repo = "ssh://test", rev = "12345", isPrivate = true, sha256 = ""},
        deps = Deps,
        release_type = release,
        builder = "buildErlangMk"
    },
    Res = rebar32nix_generator:new(App),
    Doc = prettypr:format(Res),
    ?debugFmt("~n~s~n", [Doc]),
    ok.
