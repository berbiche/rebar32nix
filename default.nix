{ pkgs ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, fetchFromGitHub ? pkgs.fetchFromGitHub
, nix ? pkgs.nix
, nix-prefetch-git ? pkgs.nix-prefetch-git
, libcap ? pkgs.libcap
, jq ? pkgs.jq
, makeWrapper ? pkgs.makeWrapper
, beamPackages ? pkgs.beam.packages.erlangR21

, customPkgs ? beamPackages
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
    in !(type == "directory" && (baseName == "result" || baseName == "_build" || baseName == ".eunit"));
  };
  checkouts = deps;
  # Compilation flags and dependencies for erlexec
  buildInputs = [ makeWrapper ] ++ lib.optional stdenv.isLinux libcap.dev;
  CXXFLAGS = lib.optionalString stdenv.isLinux ''-isystem ${libcap.dev}/include -DHAVE_CAP'';
  LDFLAGS = lib.optionalString stdenv.isLinux ''-L"${libcap.dev}/lib" -lcap'';

  postPatch = ''
    substituteInPlace ./include/rebar32nix_internal.hrl --replace "nix-prefetch-url" "${nix}/bin/nix-prefetch-url"
    substituteInPlace ./include/rebar32nix_internal.hrl --replace "nix-prefetch-git" "${nix-prefetch-git}/bin/nix-prefetch-git"
    substituteInPlace ./include/rebar32nix_internal.hrl --replace "jq" "${jq}/bin/jq"
    substituteInPlace ./include/rebar32nix_internal.hrl --replace "erlexec_port" $out/bin/exec-port
  '';

  buildPhase = "HOME=. rebar3 as default escriptize";

  checkPhase = "HOME=. rebar3 eunit";
  doCheck = true;

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
