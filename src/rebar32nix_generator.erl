-module(rebar32nix_generator).
-include("rebar32nix_internal.hrl").

-import(prettypr, [above/2, beside/2, sep/1, empty/0]).

%% API exports
-export([new/1]).

%%====================================================================
%% API functions
%%====================================================================
-spec new(app()) -> prettypr:document().
new(#app{name = AppName, vsn = Vsn} = App) ->
    Name = app_name(AppName, Vsn),
    above([
        header(),
        nest(builds(App)),
        text("in"),
        nest(sep([
            text("erlang.callPackage"),
            Name,
            text("{ }")
        ])),
        text("")
    ]).

%%====================================================================
%% Internal functions
%%====================================================================
-spec builds(app()) -> prettypr:document().
builds(#app{deps = Deps} = App) ->
    above([app(App), deps(Deps)]).

-spec app(app()) -> prettypr:document().
app(#app{name = AppName, vsn = Vsn, builder = Builder} = App) ->
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
dep_doc(#hexDep{name = Name, version = Vsn, sha256 = Sha256}) ->
    prettypr:break(
        above([
            beside([do_dep_name(Name, Vsn), text(" = "), text("fetchHex {")]),
            nest(hex_attrs(Name, Vsn, Sha256)),
            text("};")
        ])
    );
dep_doc(#gitDep{name = Name, repo = Repo, rev = Vsn, isPrivate = IsPrivate, sha256 = Sha256}) ->
    {FetchGit, GitAttrs} =
        if IsPrivate -> {"fetchGit", git_attrs(Repo, Vsn)};
           true      -> {"fetchgit", git_attrs(Repo, Vsn, Sha256)}
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
derivation(#app{builder = Builder} = App) ->
    above(
        text(Builder ++ " {"),
        above(
            nest(body(App)),
            text("};")
        )
    ).

-spec body(app()) -> prettypr:document().
body(#app{name = Name, vsn = Vsn, src = Src, deps = Deps, release_type = ReleaseType, builder = Builder}) ->
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
    lists:map(fun(A) -> dep_name(A) end, Deps).

-spec app_name(atom(), binary()) -> prettypr:document().
app_name(Name, Vsn) ->
    do_dep_name(atom_to_list(Name), Vsn).

-spec dep_name(resolvedDependency()) -> prettypr:document().
dep_name(#hexDep{name = Name, version = Vsn}) ->
    do_dep_name(Name, Vsn);
dep_name(#gitDep{name = Name, rev = Vsn}) ->
    do_dep_name(Name, Vsn).

-spec do_dep_name(binary() | string(), binary()) -> prettypr:document().
do_dep_name(Name, Vsn) ->
    beside([
        text(Name),
        text("_"),
        text(fix_version(Vsn))
    ]).

-spec quote(string()) -> prettypr:document().
quote(Value) ->
    beside([text("\""), text(Value), text("\"")]).

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
    prettypr:nest(?DEFAULT_INDENT_SIZE, Document).

-spec beside([prettypr:document()]) -> prettypr:document().
beside([Head|Tail]) ->
    lists:foldl(fun(A,B) -> prettypr:beside(B, A) end, Head, Tail).

-spec text(binary() | atom() | string()) -> prettypr:document().
text(Bin) when is_binary(Bin) ->
    prettypr:text(binary_to_list(Bin));
text(Str) when is_list(Str) ->
    prettypr:text(Str);
text(Atom) when is_atom(Atom) ->
    prettypr:text(atom_to_list(Atom)).

-spec above([prettypr:document()]) -> prettypr:document().
above([Head|Tail]) ->
    lists:foldl(fun(A,B) -> prettypr:above(B, A) end, Head, Tail).

-spec follow(prettypr:document(), prettypr:document()) -> prettypr:document().
follow(D1, D2) ->
    prettypr:follow(D1, D2, ?DEFAULT_INDENT_SIZE).

-spec fix_version(string() | binary() | atom()) -> string().
fix_version(Vsn) when is_atom(Vsn) -> fix_version(atom_to_list(Vsn));
fix_version(Vsn) when is_binary(Vsn) -> fix_version(binary_to_list(Vsn));
fix_version(Vsn) when is_list(Vsn) ->
    re:replace(Vsn, "\\.", "_", [global, {return, list}]).
