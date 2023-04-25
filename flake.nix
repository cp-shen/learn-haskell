# References:
#   https://docs.haskellstack.org/en/stable/nix_integration/
#   https://nixos.wiki/wiki/Haskell

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ghcVersion = "ghc927";
          hpkgs = pkgs.haskell.packages.${ghcVersion};
          hlib = pkgs.haskell.lib.compose;
          trv = nixpkgs.lib.trivial;
          root = ./.;
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --no-nix \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };
          myDevTools = with hpkgs; [
            stack-wrapped
            cabal-install
            haskell-language-server
            implicit-hie
            hpack
          ];

          # upstream native deps will be pulled in by nix 
          myNativeDeps = with pkgs; [
            # zlib
          ];
          myProject = returnShellEnv: extraModifiers:
            hpkgs.developPackage {
              inherit root returnShellEnv;
              modifier = (trv.flip trv.pipe) ([
                hlib.enableStaticLibraries
                hlib.justStaticExecutables
                hlib.disableExecutableProfiling
                (hlib.addBuildTools myNativeDeps)
              ] ++ extraModifiers);
            };
        in
        {
          packages.default = myProject false [ ];
          devShells.default = myProject true [
            (hlib.addBuildTools myDevTools)
            (hlib.overrideCabal (old: {
              shellHook = (old.shellHook or "") + ''
                echo "Generating .cabal file from package.yaml using hpack, \
                remember to regenerate if you change package.yaml! \
                Although nix build dont care about it, \
                its mostly for helping haskell-language-server." | \
                ${pkgs.cowsay}/bin/cowsay

                hpack

                export CABAL_CONFIG="$(pwd)/cabal_config"
              '';
            }))
          ];
        }) // {
      overlays.default = final: prev: {
        # package_name = self.packages.${final.system}.default;
      };
    };
}
