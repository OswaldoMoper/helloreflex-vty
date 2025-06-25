{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-compat, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', system, pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc96;
          devShell = {
            enable = true;
            tools = hp: {
              inherit (hp) cabal-install haskell-language-server;
            };
          };
      };
      packages = {
        default        = self'.packages.hello-reflex-vty;
        dragHelloWorld = self'.packages.dragHelloWorld;
        dragRectangle  = self'.packages.dragRectangle;
        dragEntity     = self'.packages.dragEntity;
        resizeEntity   = self'.packages.resizeEntity;
        dragNResize    = self'.packages.dragNResize;
        typicalWindow  = self'.packages.typicalWindow;
      };
      apps = {
        default        = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/helloWorld";
        };
        dragHelloWorld = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/dragHelloWorld";
        };
        dragRectangle  = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/dragRectangle";
        };
        dragEntity     = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/dragEntity";
        };
        resizeEntity   = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/resizeEntity";
        };
        dragNResize    = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/dragNResize";
        };
        typicalWindow  = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/typicalWindow";
        };
      };
    };
  };
}