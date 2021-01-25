{ pkgs ? import
    (builtins.fetchTarball {
      # Descriptive name to make the store path easier to identify
      name = "nixpkgs-unstable-2021-01-04";
      url = "https://github.com/nixos/nixpkgs/archive/56bb1b0f7a33e5d487dc2bf2e846794f4dcb4d01.tar.gz";
      sha256 = "1wl5yglgj3ajbf2j4dzgsxmgz7iqydfs514w73fs9a6x253wzjbs";
    })
    { }
, compilerVersion ? "8102" # NOTE: update this manually according to stack resolver
, compiler ? "ghc${compilerVersion}"
, ghc ? pkgs.haskell.compiler.${compiler} # passed by stack --nix
}:

with pkgs;
let
  hls = pkgs.haskell-language-server.override
    { supportedGhcVersions = [ compilerVersion ]; };
in
# build using nix-shell --run "stack --system-ghc build"
pkgs.haskell.lib.buildStackProject {
  name = "servant-exceptions";
  # src deliberatly not defined to prohibit using this derivation with nix-build (untested)
  ghc = ghc;
  buildInputs = [
    # building
    # ghc and stack already put in scope by buildStackProject
    pkgs.zlib
    # haskell tooling
    hls
    pkgs.stack
  ];
}
