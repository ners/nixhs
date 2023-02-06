{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      with builtins;
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        lib = inputs.nixpkgs.lib;
        src = inputs.nix-filter.lib {
          root = ./.;
          include = [
            "Main.hs"
            "nixhs.cabal"
          ];
        };
        removeDots = version: concatStringsSep "" (splitVersion version);
        haskellPackagesOverride = ps: ps.override {
          overrides = self: super:
            builtins.trace "GHC version: ${ps.ghc.version}"
            {
              nixhs = self.callCabal2nix "nixhs" src { };
            };
        };
        outputsFor =
          { haskellPackages
          , name ? "ghc" + removeDots haskellPackages.ghc.version
          , package ? ""
          , ...
          }:
          let ps = haskellPackagesOverride haskellPackages; in
          {
            packages.${name} = ps.${package} or ps;
            devShells.${name} = ps.shellFor {
              packages = ps: [ ps.nixhs ];
              withHoogle = true;
              nativeBuildInputs = with ps; [
                cabal-install
                fourmolu
                haskell-language-server
              ];
            };
          };
      in
      foldl' (acc: conf: lib.recursiveUpdate acc (outputsFor conf))
        {
          formatter = pkgs.nixpkgs-fmt;
        }
        ([
          {
            haskellPackages = pkgs.haskellPackages;
            name = "default";
            package = "nixhs";
          }
        ] ++ lib.pipe pkgs.haskell.packages
          [
            attrValues
            (filter (ps: ps ? ghc))
            (map (ps: { haskellPackages = ps; }))
          ])
    );
}
