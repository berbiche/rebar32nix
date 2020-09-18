{ pkgs ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, fetchFromGitHub ? pkgs.fetchFromGitHub
, nix ? pkgs.nix
, nix-prefetch-git ? pkgs.nix-prefetch-git
, libcap ? pkgs.libcap
, jq ? pkgs.jq
, makeWrapper ? pkgs.makeWrapper
, beamPackages ? pkgs.beam.packages.erlangR21

, customPkgs ?
    (if builtins.pathExists ../nixgear
     then (import ../nixgear { inherit pkgs; }).packages
     else beamPackages)
, erlang ? customPkgs.erlang
, fetchHex ? customPkgs.fetchHex
, rebar3Relx ? customPkgs.rebar3Relx
, buildRebar3 ? customPkgs.buildRebar3
, fetchRebar3Deps ? customPkgs.fetchRebar3Deps
}:

let
  inherit (stdenv) lib;

  deps = fetchRebar3Deps {
    name = "rebar32nix";
    version = "0.1.0";
    sha256 = "dLEP2t8eNel2m75esBjF4yHKQLcIAyBFwFZm1vL/DFc=";

    src = "${./.}/rebar.lock";

    postInstall = ''
      mkdir -p $out/_checkouts
      cp -rT $out/lib/erlang/lib $out/_checkouts
      cp -rT $out/lib/erlang/plugins $out/_checkouts
    '';
  };
  
in
rebar3Relx rec {
  name = "rebar32nix";
  version = "0.1.0";
  releaseType = "escript";

  src = lib.cleanSourceWith {
    src = lib.cleanSource ./.;
    filter = name: type: let
      baseName = baseNameOf (toString name);
    in !(type == "directory" && (baseName == "result" || baseName == "_build"));
  };
  # For erlexec
  buildInputs = [ makeWrapper ] ++ lib.optional stdenv.isLinux libcap.dev;
  checkouts = deps;
  CXXFLAGS = ''-isystem ${libcap.dev}/include -DHAVE_CAP'';
  LDFLAGS = ''-L"${libcap.dev}/lib" -lcap'';

  postPatch = ''
    substituteInPlace ./src/rebar32nix.erl --replace "nix-prefetch-url" "${nix}/bin/nix-prefetch-url"
    substituteInPlace ./src/rebar32nix.erl --replace "nix-prefetch-git" "${nix-prefetch-git}/bin/nix-prefetch-git"
    substituteInPlace ./src/rebar32nix.erl --replace "jq" "${jq}/bin/jq"
    substituteInPlace ./src/rebar32nix.erl --replace "erlexec_port" $out/bin/exec-port
  '';

  buildPhase = "HOME=. rebar3 as default escriptize";

  postInstall = ''
    cp --preserve=mode _checkouts/erlexec/priv/${stdenv.buildPlatform.config}/exec-port $out/bin/exec-port
    wrapProgram $out/bin/exec-port \
      --set-default SHELL ${stdenv.shell} \
      --prefix PATH : ${lib.makeBinPath [ nix nix-prefetch-git jq ]}
  '';

  meta = with stdenv.lib; {
    description = "A script to generate a Nix expression out of a rebar3.lock file";
    homepage = "https://github.com/ankhers/rebar32nix";
    license = licenses.asl20;
    platform = platforms.all;
  };
}
