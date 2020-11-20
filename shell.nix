{ pkgs ? import
    (builtins.fetchTarball {
      # Descriptive name to make the store path easier to identify
      name = "nixpkgs-unstable-2018-09-12";
      url = "https://github.com/nixos/nixpkgs/archive/2247d824fe07f16325596acc7faa286502faffd1.tar.gz";
      sha256 = "09jzdnsq7f276cfkmynqiaqg145k8z908rlr0170ld1sms1a83rw";
    })
    { }
, compilerVersion ? "884" # NOTE: update this manually according to stack resolver
, compiler ? "ghc${compilerVersion}"
, ghc ? pkgs.haskell.compiler.${compiler} # passed by stack --nix
}:

with pkgs;

# build using nix-shell --run "stack --system-ghc build"
pkgs.mkShell rec {
  buildInputs = [
    ghc
    hls
    pkgs.zlib
    pkgs.stack
  ];

  # Make libraries available to ld, such that template haskell compilation
  # (ghci) can find them.
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;

  # REVIEW alternative to the above
  # shellHook = ''
  #   eval $(grep export ${ghc}/bin/ghc)
  #   export LD_LIBRARY_PATH="${pkgs.zlib}/lib";
  # '';
}
# # build using stack --nix
# pkgs.haskell.lib.buildStackProject {
#   name = "servant-exceptions";
#   ghc = ghc;
#   buildInputs = [ pkgs.zlib ];
# }
