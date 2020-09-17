-module(rebar32nix).

%% API exports
-export([main/1]).

-define(HEX_REGISTRY_URI, "https://repo.hex.pm/tarballs").
-define(NIX_PREFETCH_URL, "nix-prefetch-url").
-define(NIX_PREFETCH_GIT, "nix-prefetch-git").
-define(JQ, "jq").
-define(ERLEXEC_PORT, "erlexec_port").
-define(DEFAULT_ERLEXEC_OPTS, [sync, stdout, {stderr, null}, {kill_timeout, 120}, {nice, 10}]).

-type dependency() :: {hex, Name::string(), Vsn::string()}
                   |  {git, Name::string(), Repo::string(), Vsn::string()}.


main(Args) ->
    %% io:format("Args: ~p~n", [Args]),
    {ok, {Opts, _}} = getopt:parse(opts_list(), Args),
    case proplists:get_bool(help, Opts) of
        true -> getopt:usage(opts_list(), "rebar32nix");
        false ->
            case proplists:get_value(file, Opts) of
                undefined ->
                    getopt:usage(opts_list(), "rebar32nix"),
                    erlang:halt(1);
                File ->
                    Path = filename:absname(File),
                    case filelib:is_dir(Path) of
                        false ->
                            io:format("Invoke rebar32nix on the root folder of the project~n"),
                            erlang:halt(1);
                        true ->
                            ReleaseType = proplists:get_value(release_type, Opts),
                            Src = filename:absname_join(Path, "src"),
                            LockFile = filename:absname_join(Path, "rebar.lock"),
                            log_stderr("src is ~s~n", [Src]),
                            log_stderr("lockfile is ~s~n", [LockFile]),
                            app(Src, LockFile, ReleaseType, Args)
                    end
            end
    end,
    erlang:halt(0).

-spec app(string(), string(), string(), tuple()) -> ok.
app(ProjectSource, LockFile, ReleaseType, Args) ->
    _ = application:load(rebar32nix),
    {ok, {application, AppName, List}} = app_src(ProjectSource),
    Vsn = proplists:get_value(vsn, List),
    {ok, L} = exec:start_link([{portexe, ?ERLEXEC_PORT}]),
    Deps = fetch_deps(LockFile),
    exec:stop_and_wait(L, 5000),
    App = #{
            name => AppName,
            vsn => Vsn,
            src => ProjectSource,
            deps => Deps,
            release_type => ReleaseType
           },
    Doc = prettypr:above(header(Args), generator:new(App)),
    io:format("~s", [prettypr:format(Doc)]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
opts_list() ->
    [
     %% {Name,      ShortOpt,  LongOpt,        ArgSpec,         HelpMsg}
     {help,         $h,        "help",         undefined,       "Print this help."},
     {version,      $v,        "version",      undefined,       "Show version information."},
     {release_type, undefined, "release-type", {atom, release}, "Generate either a release or an escript."},
     {file,         undefined, undefined,      string,          "Input file"}
    ].

-spec header([string()]) -> prettypr:document().
header(Args) ->
    ArgsDoc = lists:map(fun prettypr:text/1, Args),
    {ok, Vsn} = application:get_key(rebar32nix, vsn),
    Docs = [
            prettypr:text("# Generated by rebar32nix"),
            prettypr:beside(prettypr:text(Vsn), prettypr:text(":")),
            prettypr:text("rebar32nix")
           | ArgsDoc
           ],
    Docs1 = lists:join(prettypr:text(" "), Docs),
    lists:foldr(fun prettypr:beside/2, prettypr:empty(), Docs1).

-spec app_src(string()) -> {ok, term()}.
app_src(ProjectSource) ->
    {ok, Files} = file:list_dir(ProjectSource),
    [FileName] = lists:filter(fun(F) ->
                                      case re:run(F, "\.app\.src$") of
                                          {match, _} -> true;
                                          _ -> false
                                      end
                              end, Files),
    FullPath = unicode:characters_to_list([ProjectSource, "/", FileName]),
    file:script(FullPath).

-spec fetch_deps(Path::string() | {[dependency()], [dependency()]}) -> [generator:resolvedDependency()].
fetch_deps({PublicDeps, PrivateDeps}) ->
    % Handle the "non-recursive" deps first. What is meant by non-recursive
    % is that these deps. will not depend on private dependencies.
    Deps1 = lists:map(fun(Dep) ->
                              case fetch_dep(Dep) of
                                  {_, GitDep} -> GitDep;
                                  PublicDep -> PublicDep
                              end
                      end, PublicDeps),
    % We now need to fetch all private dependencies, which is done recursively
    Deps2 = lists:map(fun(Dep) ->
                              case fetch_dep(Dep) of
                                  {Path, {git, Name, _, _}} -> {Name, fetch_deps(Path)};
                                  PublicDep -> PublicDep
                              end
                      end, PrivateDeps),
    lists:append(Deps1, Deps2);
fetch_deps(Path) ->
    log_stderr("Inspecting dependencies~n"),
    Deps = get_deps_list(Path),
    log_stderr("Deps: ~p~n", [Deps]),
    AllDeps = fetch_deps(Deps),
    AllDeps.

-spec fetch_dep(dependency()) -> generator:resolvedDependency().
fetch_dep({hex, Name, Vsn }) ->
    Sha256 = hex_sha256(Name, Vsn),
    {hex, Name, Vsn, Sha256};
fetch_dep({git, Name, Repo, Vsn}) ->
    {Path, Sha256} = git_sha256(Repo, Vsn),
    {Path, {git, Name, Repo, Vsn, Sha256}}.

-spec get_deps_list(string) -> {[dependency()], [dependency()]}.
get_deps_list(Filename) ->
    DepsList = case filelib:is_regular(Filename) of
                   true ->
                       {ok, [{_RebarLockVersion, Deps}|_]} = file:consult(Filename),
                       [{binary_to_list(Name), Source, Level} || {Name, Source, Level} <- Deps];
                   false -> []
               end,
    log_stderr("DepsList: ~p~n", [DepsList]),
    PublicDeps = lists:map(fun convert_dep/1, DepsList),
    PrivateDepsToVisit = lists:filter(fun is_private_git_repo/1, DepsList),
    {PublicDeps, PrivateDepsToVisit}.

-spec convert_dep({string(), tuple(), any()}) -> dependency().
convert_dep({Name, {pkg, _, Vsn}, _}) ->
    {hex, Name, binary_to_list(Vsn)};
convert_dep({Name, {git, Repo, Meta}, _}) ->
    Vsn = case Meta of
              {ref, Ref} -> Ref;
              {branch, Branch} -> Branch;
              {tag, Tag} -> Tag
          end,
    {git, Name, Repo, binary_to_list(Vsn)}.

-spec is_private_git_repo(dependency()) -> boolean().
is_private_git_repo({git, _, "ssh://" ++ _, _}) -> true;
is_private_git_repo(_) -> false.

%%====================================================================
%% Fetcher functions
%%====================================================================
-spec hex_sha256(string(), string()) -> string().
hex_sha256(Name, Vsn) ->
    %% "https://repo.hex.pm/tarballs/${pkg}-${version}.tar";
    log_stderr("hex_sha256: fetching ~s~n", [hex_url(Name, Vsn)]),
    {ok, [{stdout, Output}]} = exec:run([?NIX_PREFETCH_URL, hex_url(Name, Vsn)], ?DEFAULT_ERLEXEC_OPTS),
    [Sha | _] = Output,
    OutSha = binary_to_list(iolist_to_binary(Sha)),
    log_stderr("hex_sha256: fetched ~s with hash ~s~n", [hex_url(Name, Vsn), OutSha]),
    OutSha.

-spec git_sha256(string(), string()) -> {Path::string(), Sha256::string()}.
git_sha256(Repo, Vsn) ->
    log_stderr("git_sha256: fetching ~s, rev: ~s", [Repo, Vsn]),
    {ok, [{stdout, Output}]} = exec:run([
            ?NIX_PREFETCH_GIT, Repo, "--rev", Vsn,
            "|", ?JQ, "-r", "'.path,.sha256'"
        ], ?DEFAULT_ERLEXEC_OPTS),
    [Path, Sha256] = lists:map(fun binary_to_list/1, Output),
    {Path, Sha256}.

-spec hex_url(binary(), binary()) -> string().
hex_url(Name, Vsn) ->
    lists:concat([?HEX_REGISTRY_URI, "/", Name, "-", Vsn, ".tar"]).

log_stderr(String) -> log_stderr(String, []).
log_stderr(String, Args) -> io:format(standard_error, String, Args).
