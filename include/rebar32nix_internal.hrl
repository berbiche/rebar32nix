%% records
-record(app, {
    name         = undefined :: atom(),
    vsn          = undefined :: string(),
    src          = undefined :: string(),
    deps         = undefined :: [resolvedDependency()],
    release_type = undefined :: atom(),
    builder      = undefined :: string()
}).

-record(hexDep, {
    name    :: string(),
    version :: string(),
    %% sha256 is known only after fetching a dependency
    sha256  = undefined :: string()
}).

-record(gitDep, {
    name      :: string(),
    repo      :: string(),
    rev       :: string(),
    isPrivate :: boolean(),
    %% sha256 is known only after fetching a dependency
    sha256    = undefined :: string()
}).

%% types
-type app() :: #app{}.
-type hexDep() :: #hexDep{}.
-type gitDep() :: #gitDep{}.
%% An erlang dependency for the Nix derivation
-type resolvedDependency() :: hexDep() | gitDep().
%% A rebar.lock dependency that has not been resolved
-type dependency() :: {hex, Name::string(), Vsn::string()}
                   |  {git, Name::string(), Repo::string(), Vsn::string()}.

%% defines
-define(DEFAULT_INDENT_SIZE, 2).
-define(HEX_REGISTRY_URI, "https://repo.hex.pm/tarballs").
-define(NIX_PREFETCH_URL, "nix-prefetch-url").
-define(NIX_PREFETCH_GIT, "nix-prefetch-git").
-define(JQ, "jq").
-define(ERLEXEC_PORT, "erlexec_port").
-define(DEFAULT_ERLEXEC_OPTS, [sync, stdout, stderr, {kill_timeout, 120}, {nice, 10}]).
