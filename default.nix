{ pkgs ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, fetchFromGitHub ? pkgs.fetchFromGitHub
, nix ? pkgs.nix
, nix-prefetch-git ? pkgs.nix-prefetch-git
, libcap ? pkgs.libcap
, beamPackages ? pkgs.beam.packages.erlangR21
, erlang ? beamPackages.erlang
, fetchHex ? beamPackages.fetchHex
, rebar3Relx ? beamPackages.rebar3Relx
, buildRebar3 ? beamPackages.buildRebar3
}:

let
  inherit (stdenv) lib;
  inherit (beamPackages) pc;

  getopt = fetchHex {
    pkg = "getopt";
    version = "1.0.1";
    sha256 = "U+Grg7nOtlyWctPno1uAkum9ybPugHIUcaFhwQxZlZw=";
  };
  erlexec = buildRebar3 rec {
    name = "erlexec";
    version = "1.17.5";
    compilePorts = true;
    enableDebugInfo = true;

    src = fetchHex {
      pkg = "erlexec";
      version = "1.17.5";
      sha256 = "uiLczMkU7xnsoQAtwWXFhBRcdM9XfJ11Tm1KyNOBAv8=";
    };

    # buildInputs = [ ];

    preConfigure = ''
      mkdir -p $out/_checkouts/pc
      cp -v --no-preserve=mode -R ${pc}/lib/erlang/lib/${pc.name} $out/_checkouts/pc
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
  buildInputs = lib.optional stdenv.isLinux libcap.dev;

  postPatch = ''
    substituteInPlace ./src/rebar32nix.erl --replace "nix-prefetch-url" "${nix}/bin/nix-prefetch-url"
    substituteInPlace ./src/rebar32nix.erl --replace "nix-prefetch-git" "${nix-prefetch-git}/bin/nix-prefetch-git"
  '';

  checkouts = stdenv.mkDerivation {
    name = "rebar32nix-checkouts";
    inherit src;
    phases = "installPhase";
    installPhase = ''
      mkdir -p $out/_checkouts
      mkdir -p $out/_build/default/lib

      cp --no-preserve=mode -R ${pc} $out/_checkouts/pc
      cp --no-preserve=mode -R ${getopt} $out/_checkouts/getopt
      cp --no-preserve=mode -R ${erlexec.out} $out/_checkouts/erlexec

      for i in _checkouts/* ; do
        ln -sv _checkouts/$i $(pwd)/_build/default/lib
      done
    '';
  };

  meta = with stdenv.lib; {
    description = "A script to generate a Nix expression out of a rebar3.lock file";
    homepage = "https://github.com/ankhers/rebar32nix";
    license = licenses.asl20;
    platform = platforms.all;
  };
}
