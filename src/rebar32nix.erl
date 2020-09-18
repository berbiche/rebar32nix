-module(rebar32nix).

%% API exports
-export([main/1]).

-define(HEX_REGISTRY_URI, "https://repo.hex.pm/tarballs").
-define(NIX_PREFETCH_URL, "nix-prefetch-url").
-define(NIX_PREFETCH_GIT, "nix-prefetch-git").
-define(JQ, "jq").
-define(ERLEXEC_PORT, "erlexec_port").
-define(DEFAULT_ERLEXEC_OPTS, [sync, stdout, stderr, {kill_timeout, 120}, {nice, 10}]).

-type dependency() :: {hex, Name::string(), Vsn::string()}
                   |  {git, Name::string(), Repo::string(), Vsn::string()}.

%% prettypr emits tabs when nesting == 8, so we replace them with spaces
-define(REPLACE_TABS_WITH_SPACES(S), lists:flatten(string:replace(S, "\t", "\s\s\s\s\s\s\s\s", all))).
-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE >= 23).
  fix_prettypr_document_tabs(S) -> S.
  -else.
  fix_prettypr_document_tabs(S) -> ?REPLACE_TABS_WITH_SPACES(S).
  -endif.
-else.
fix_prettypr_document_tabs(S) -> ?REPLACE_TABS_WITH_SPACES(S).
-endif.

-spec main(term()) -> no_return().
main(Args) ->
    log_stderr("Args: ~p~n", [Args]),
    {ok, {Opts, _}} = getopt:parse(opts_list(), Args),
    case proplists:get_bool(help, Opts) of
        true -> getopt:usage(opts_list(), "rebar32nix");
        false ->
            Path = get_root_path(Opts),
            log_stderr("input is ~s~n", [Path]),
            Src = get_src_dir(Path),
            log_stderr("src is ~s~n", [Src]),
            LockFile = get_lock_file(Path),
            log_stderr("lockfile is ~s~n", [LockFile]),
            {Fd, OutFile} = get_out_file(Opts),
            log_stderr("outfile is ~s~n", [OutFile]),
            ReleaseType = proplists:get_value(release_type, Opts),
            Builder = proplists:get_value(builder, Opts),

            Doc = app(Path, Src, LockFile, ReleaseType, Builder, Args),
            DocTabsReplaced = fix_prettypr_document_tabs(Doc),
            io:format(Fd, "~s", [DocTabsReplaced]),
            log_stderr("Generation complete~n"),
            ok
    end,
    erlang:halt(0).

-spec get_root_path(tuple()) -> no_return() | string().
get_root_path(Opts) ->
    case proplists:get_value(file, Opts) of
        undefined ->
            getopt:usage(opts_list(), "rebar32nix"),
            erlang:halt(1);
        File ->
            Path = filename:absname(File),
            case filelib:is_dir(Path) of
                true -> Path;
                false ->
                    io:format("Invoke rebar32nix on the root folder of the project~n"),
                    erlang:halt(1)
            end
    end.

-spec get_src_dir(string()) -> no_return() | string().
get_src_dir(Path) ->
    SrcDir = filename:absname_join(Path, "./src"),
    case filelib:is_dir(SrcDir) of
        true -> SrcDir;
        false -> 
            io:format("Could not find 'src' folder in '~s', make sure the directory exists", [Path]),
            erlang:halt(1)
    end.

-spec get_lock_file(string()) -> no_return() | string().
get_lock_file(Path) ->
    LockFile = filename:absname_join(Path, "./rebar.lock"),
    case filelib:is_regular(LockFile) of
        true -> LockFile;
        false -> 
            io:format("Could not find rebar.lock in '~s', make sure the file exists", [Path]),
            erlang:halt(1)
    end.

-spec get_out_file(tuple()) -> atom() | file:io_device().
get_out_file(Opts) ->
    case proplists:get_value(out_file, Opts) of
        undefined -> {standard_io, standard_io};
        Out ->
            Args = [write, {encoding, utf8}],
            case file:open(filename:absname(Out), Args) of
                {ok, Fd} -> {Fd, filename:absname(Out)};
                {error, _} -> {standard_io, standard_io}
            end
    end.

-spec app(string(), string(), string(), string(), string(), tuple()) -> string().
app(ProjectRoot, ProjectSource, LockFile, ReleaseType, Builder, Args) ->
    _ = application:load(rebar32nix),
    {ok, {application, AppName, List}} = app_src(ProjectSource),
    Vsn = proplists:get_value(vsn, List),

    {ok, L} = exec:start_link([{portexe, ?ERLEXEC_PORT}]),
    Deps = fetch_deps(LockFile),
    log_stderr("Fetched all dependencies! Shutting down remaining fetchers...~n"),
    exec:stop_and_wait(L, 5000),
    log_stderr("Generating~n"),

    App = #{
            name => AppName,
            vsn => Vsn,
            src => ProjectRoot,
            deps => Deps,
            release_type => ReleaseType,
            builder => Builder
           },
    Doc = prettypr:above(header(Args), generator:new(App)),
    prettypr:format(Doc).

%%====================================================================
%% Internal functions
%%====================================================================
opts_list() ->
    [
     %% {Name,      ShortOpt,  LongOpt,        ArgSpec,                HelpMsg}
     {help,         $h,        "help",         undefined,              "Print this help."},
     {version,      $v,        "version",      undefined,              "Show version information."},
     {release_type, undefined, "release-type", {atom, release},        "Generate either a release or an escript."},
     {builder,      undefined, "builder",      {string, "rebar3Relx"}, "Derivation builder to use"},
     {out_file,     $o,        "out",          string,                 "Output file"},
     {file,         undefined, undefined,      string,                 "Input file"}
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
fetch_dep({hex, Name, Vsn}) ->
    Sha256 = hex_sha256(Name, Vsn),
    {hex, Name, Vsn, Sha256};
fetch_dep({git, Name, Repo, Vsn, IsPrivateRepo}) ->
    {Path, Sha256} = git_sha256(Repo, Vsn),
    {Path, {git, Name, Repo, Vsn, IsPrivateRepo, Sha256}}.

-spec get_deps_list(string) -> {[dependency()], [dependency()]}.
get_deps_list(Filename) ->
    DepsList = case filelib:is_regular(Filename) of
                   true ->
                       {ok, [{_RebarLockVersion, Deps}|_]} = file:consult(Filename),
                       [{binary_to_list(Name), Source, Level} || {Name, Source, Level} <- Deps];
                   false -> []
               end,
    PublicDeps = lists:map(fun convert_dep/1, DepsList),
    PrivateDepsToVisit = lists:filter(fun is_private_git_repo/1, DepsList),
    {PublicDeps, PrivateDepsToVisit}.

-spec convert_dep({string(), tuple(), any()}) -> dependency().
convert_dep({Name, {pkg, _, Vsn}, _}) ->
    {hex, Name, binary_to_list(Vsn)};
convert_dep({Name, {git, Repo, Meta}, _}) ->
    Vsn = case Meta of
              {ref, Ref} -> Ref;
              {branch, Branch} -> "refs/heads/" ++ Branch;
              {tag, Tag} -> "refs/tags/" ++ Tag
          end,
    F = {git, Name, Repo, Vsn},
    erlang:append_element(F, is_private_git_repo(F)).

-spec is_private_git_repo(dependency()) -> boolean().
is_private_git_repo({git, _, Repo, _}) -> string:find(Repo, "ssh://", leading) =/= nomatch;
is_private_git_repo(_) -> false.

%%====================================================================
%% Fetcher functions
%%====================================================================
-spec hex_sha256(string(), string()) -> string().
hex_sha256(Name, Vsn) ->
    %% "https://repo.hex.pm/tarballs/${pkg}-${version}.tar";
    log_stderr("hex_sha256: fetching ~s~n", [hex_url(Name, Vsn)]),

    {ok, [{stdout, Output} | _]} = exec:run([
        ?NIX_PREFETCH_URL, hex_url(Name, Vsn)
    ], ?DEFAULT_ERLEXEC_OPTS),

    [Sha | _] = Output,
    OutSha = string:trim(binary_to_list(Sha)),

    log_stderr("hex_sha256: fetched ~s with hash ~s~n", [hex_url(Name, Vsn), OutSha]),

    OutSha.

-spec git_sha256(string(), string()) -> {Path::string(), Sha256::string()}.
git_sha256(Repo, Vsn) ->
    log_stderr("git_sha256: fetching ~s, rev: ~s~n", [Repo, Vsn]),
    
    {ok, [{stdout, [Output]} | _]} = exec:run([
        ?NIX_PREFETCH_GIT, "--url", "'" ++ Repo ++ "'", "--rev", Vsn
    ], ?DEFAULT_ERLEXEC_OPTS),

    #{<<"path">> := Path, <<"sha256">> := Sha256} = jsone:decode(Output),
    log_stderr("git_sha256: fetched ~s, Path: ~s, Sha256: ~s~n", [Repo, Path, Sha256]),
    {binary_to_list(Path), binary_to_list(Sha256)}.

-spec hex_url(binary(), binary()) -> string().
hex_url(Name, Vsn) ->
    lists:concat([?HEX_REGISTRY_URI, "/", Name, "-", Vsn, ".tar"]).

log_stderr(String) -> log_stderr(String, []).
log_stderr(String, Args) -> io:format(standard_error, String, Args).
