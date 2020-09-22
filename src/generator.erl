-module(generator).

-import(prettypr, [above/2, beside/2, sep/1, empty/0]).

%% API exports
-export([new/1]).

-type(app() :: #{
    name := atom(),
    vsn := string(),
    src := string(),
    deps := [{binary(), binary()}],
    release_type := atom(),
    builder := string()
}).
-export_type([app/0]).

-type hexDep() :: {hex, Name::string(), Vsn::string(), Sha256::string()}.
-type gitDep() :: {git, Name::string(), Repo::string(), Rev::string(), IsPrivateRepo::boolean(), Sha256::string()}.

-type resolvedDependency() :: hexDep()
                            | gitDep()
                            | {Name::string, [resolvedDependency()]}.
-export_type([resolvedDependency/0]).


-define(default_indent_size, 2).

%%====================================================================
%% API functions
%%====================================================================
-spec new(app()) -> prettypr:document().
new(#{name := AppName, vsn := Vsn} = App) ->
    Name = app_name(AppName, Vsn),
    above([
        header(),
        nest(builds(App)),
        text("in"),
        sep([
            text(" erlang.callPackage"),
            Name,
            text("{ }")
        ])
    ]).

%%====================================================================
%% Internal functions
%%====================================================================
-spec builds(app()) -> prettypr:document().
builds(#{deps := Deps} = App) ->
    above([app(App), deps(Deps)]).

-spec app(app()) -> prettypr:document().
app(#{name := AppName, vsn := Vsn, builder := Builder} = App) ->
    Name = app_name(AppName, Vsn),
    above([
        beside(Name, text(" = { " ++ Builder ++ " }:")),
        nest(derivation(App)),
        text("")
    ]).

-spec header() -> prettypr:document().
header() ->
    above([
        text("{ stdenv, erlang, fetchHex, fetchgit }:"),
        text(""),
        text("let"),
        nest(sep([text("inherit"), text("(builtins)"), text("fetchGit;")]))
        %empty()
    ]).

-spec deps([resolvedDependency()]) -> prettypr:document().
deps(Deps) ->
    lists:foldr(fun(Dep, Acc) ->
        above(dep_doc(Dep), Acc)
    end, empty(), Deps).

-spec dep_doc(resolvedDependency()) -> prettypr:document().
dep_doc({hex, Name, Vsn, Sha256}) ->
    prettypr:break(
        above([
            beside([do_dep_name(Name, Vsn), text(" = "), text("fetchHex {")]),
            nest(hex_attrs(Name, Vsn, Sha256)),
            text("};")
        ])
    );
dep_doc({git, Name, Repo, Vsn, IsPrivateRepo, Sha256}) ->
    {GitAttrs, FetchGit} =
        if IsPrivateRepo -> {git_attrs(Repo, Vsn), "fetchGit"};
           true          -> {git_attrs(Repo, Vsn, Sha256), "fetchgit"}
        end,
    prettypr:break(
        above([
            beside([do_dep_name(Name, Vsn), text(" = "), text(FetchGit ++ " {")]),
            nest(GitAttrs),
            text("};")
        ])
    ).

-spec hex_attrs(string(), string(), string()) -> prettypr:document().
hex_attrs(Name, Vsn, Sha256) ->
    above([
        kv("pkg", quote(Name)),
        kv("version", quote(Vsn)),
        kv("sha256", quote(Sha256))
    ]).

%% For private git repos
-spec git_attrs(string(), string()) -> prettypr:document().
git_attrs(Repo, GitVsn) ->
    above([
        kv("url", quote(Repo)),
        kv("rev", quote(GitVsn))
    ]).

-spec git_attrs(string(), string(), string()) -> prettypr:document().
git_attrs(Repo, GitVsn, Sha256) ->
    above([
        kv("url", quote(Repo)),
        kv("rev", quote(GitVsn)),
        kv("sha256", quote(Sha256))
    ]).

-spec derivation(app()) -> prettypr:document().
derivation(#{builder := Builder} = App) ->
    above(
        text(Builder ++ " {"),
        above(
            nest(body(App)),
            text("};")
        )
    ).

-spec body(app()) -> prettypr:document().
body(#{name := Name, vsn := Vsn, src := Src, deps := Deps, release_type := ReleaseType, builder := Builder}) ->
    ReleaseType2 = case Builder of
        "rebar3Relx" -> kv("releaseType", quote(atom_to_list(ReleaseType)));
        _            -> empty()
    end, 
    above([
        kv("name", quote(atom_to_list(Name))),
        kv("version", quote(Vsn)),
        kv("src", Src),
        ReleaseType2,
        deps_list(Deps)
    ]).

-spec deps_list([resolvedDependency()]) -> prettypr:document().
deps_list(Deps) ->
    case deps_names(Deps) of
        [] ->
            empty();
        DepsDocs ->
            above([
                text("beamDeps = ["),
                nest(above(DepsDocs)),
                text("];")
            ])
    end.

-spec deps_names([{binary(), binary()}]) -> [prettypr:document()].
deps_names(Deps) ->
    lists:map(fun dep_name/1, Deps).

-spec app_name(atom(), binary()) -> prettypr:document().
app_name(Name, Vsn) ->
    do_dep_name(atom_to_list(Name), Vsn).

-spec dep_name(resolvedDependency()) -> prettypr:document().
dep_name({hex, Name, Vsn, _Sha256}) ->
    do_dep_name(Name, Vsn);
dep_name({git, Name, _Repo, Vsn, _Path, _Sha256}) ->
    do_dep_name(Name, string:slice(Vsn, 0, 8)).

-spec do_dep_name(binary() | string(), binary()) -> prettypr:document().
do_dep_name(Name, Vsn) ->
    beside([
        text(Name),
        text("_"),
        text(fix_version(Vsn))
    ]).

-spec quote(string()) -> prettypr:document().
quote(Value) ->
    beside(text("\""), beside(text(Value), text("\""))).

-spec kv(string(), string() | prettypr:document()) -> prettypr:document().
kv(Key, Value) when is_list(Value) ->
    beside(
        follow(beside(text(Key), text(" =")), text(Value)),
        text(";")
    );
kv(Key, Value) ->
    beside(
        follow(beside(text(Key), text(" =")), Value),
        text(";")
    ).

-spec nest(prettypr:document()) -> prettypr:document().
nest(Document) ->
    prettypr:nest(?default_indent_size, Document).

-spec beside([prettypr:document()]) -> prettypr:document().
beside(List) when is_list(List) ->
    lists:foldr(fun prettypr:beside/2, empty(), List).

-spec text(binary() | atom() | string()) -> prettypr:document().
text(Bin) when is_binary(Bin) ->
    prettypr:text(binary_to_list(Bin));
text(Str) when is_list(Str) ->
    prettypr:text(Str);
text(Atom) when is_atom(Atom) ->
    prettypr:text(atom_to_list(Atom)).

-spec above([prettypr:document()]) -> prettypr:document().
above(List) when is_list(List) ->
    lists:foldr(fun prettypr:above/2, empty(), List).

-spec follow(prettypr:document(), prettypr:document()) -> prettypr:document().
follow(D1, D2) ->
    prettypr:follow(D1, D2, ?default_indent_size).

-spec fix_version(string() | binary() | atom()) -> string().
fix_version(Vsn) when is_atom(Vsn) -> fix_version(atom_to_list(Vsn));
fix_version(Vsn) when is_binary(Vsn) -> fix_version(binary_to_list(Vsn));
fix_version(Vsn) when is_list(Vsn) ->
    re:replace(Vsn, "\\.", "_", [global, {return, list}]).
