{ pkgs ? import <nixpkgs> { }
, customPkgs ? (import ../nixgear { inherit pkgs; }).packages
, erlang ? customPkgs.erlang
, erlangPackages
, fetchHex ? customPkgs.fetchHex
, rebar3Relx ? customPkgs.rebar3Relx
, buildRebar3 ? customPkgs.buildRebar3
, fetchRebar3Deps ? customPkgs.fetchRebar3Deps
}:

let
  inherit (pkgs) lib;
  
  package = 
    pkgs.callPackage ./deps.nix {
      inherit rebar3Relx fetchHex;
      erlang = erlangPackages;
      fetchGit = builtins.fetchGit;
    };
in
  package.overrideAttrs(old: {
    src = lib.cleanSourceWith {
      src = lib.cleanSource ./.;
      filter = name: type: let
        baseName = baseNameOf (toString name);
      in !(type == "directory" && (baseName == "result" || baseName == "_build"));
    };
  })