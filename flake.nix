{
  description = "configs";
  # nix --experimental-features "nix-command flakes" --allow-import-from-derivation build .#profile -v --print-build-logs

  # cachix use cuda-maintainers

  inputs = {
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "empty";
    };

    crate2nix = {
      url = "github:nix-community/crate2nix";
      inputs.flake-utils.follows = "flake-utils";
    };

    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        purescript-overlay.follows = "empty";
        pyproject-nix.follows = "empty";
      };
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "empty";
      inputs.nixpkgs-stable.follows = "empty";
    };

    empty = {
      url = "github:sarcasticadmin/empty-repo";
      flake = false;
    };

    flake-compat = {
      # https://github.com/nix-community/nixd/blob4ab27e5595ea3e9c5e659065b85c7b66f87384d9omain/docs/examples/flake/flake.nix
      url = "github:inclyc/flake-compat";
      flake = false;
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs-lib";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      # inputs.nixpkgs.follows = "nixpkgs";
    };

    mini-compile-commands = {
      # nix flake update mini-compile-commands
      # url = "github:danielbarter/mini_compile_commands";
      url = "github:Alan-Chen99/mini_compile_commands";
      flake = false;
    };

    nix = {
      url = "github:edolstra/nix/19ec1c9fd4d4bf6e941b046b8549ba2a1a690937";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-regression.follows = "empty";
        flake-compat.follows = "flake-compat";
      };
    };

    nix-filter.url = "github:numtide/nix-filter";

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixd = {
      url = "github:nix-community/nixd/release/1.2";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };

    nixpkgs = {
      # nix flake lock --update-input nixpkgs
      # url = "github:nixos/nixpkgs/nixos-23.11";
      url = "github:nixos/nixpkgs/c8e74c2f83fe12b4e5a8bd1abbc090575b0f7611";
    };

    nixpkgs-lib.url = "github:nixos/nixpkgs/c8e74c2f83fe12b4e5a8bd1abbc090575b0f7611?dir=lib";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nixpkgs21-11.url = "github:nixos/nixpkgs/nixos-21.11";

    nixpkgs22-11.url = "github:nixos/nixpkgs/nixos-22.11";

    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nix-github-actions.follows = "empty";
        nixpkgs.follows = "empty";
        systems.follows = "systems";
        treefmt-nix.follows = "empty";
      };
    };

    racket-fmt = {
      url = "github:sorawee/fmt";
      flake = false;
    };

    racket2nix = {
      # url = "github:fractalide/racket2nix";
      # url = "github:corpix/racket2nix";
      url = "github:dwarfmaster/racket2nix";
      flake = false;
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    schemat = {
      url = "github:raviqqe/schemat";
      flake = false;
    };

    systems.follows = "flake-utils/systems";
  };

  outputs = inputs @ {
    self,
    flake-utils,
    nixpkgs,
    nix-index-database,
    nixd,
    # nix-profile-names,
    crate2nix,
    ...
  }: let
    forsystem-impl = system: outputs-system: rec {
      package = import ./nix/package-bootstrap.nix {};

      default =
        package.import-package ./nix/default.nix {
          flakes = inputs;
          self = default;
          system = system;
        } (
          final: prev: let
            appendversion = deriv:
              deriv.overrideAttrs (old: {
                version = "${old.version or ""}" + final.version;
                name = "${old.name}-${final.version}";
              });
            pkgs-versioned = {
              inherit
                (prev)
                cxxtools
                flake-registry-file
                fonts
                js
                nixtools
                profile
                profile-root
                pythonlibs
                pythontools
                ;
            };
            pkgs-other = {
              inherit
                (prev)
                emacs
                emacs-test
                python-all
                rust-src-hack
                scmindent
                test
                test2
                youtube-dl
                ;

              python = prev.poetrypython.python;

              # inherit
              #   (final.python.pkgs)
              #   youtube-dl
              #   ;

              schemat = final.craneLib.buildPackage {
                src = inputs.schemat;
              };

              # rackt-test = final.deps.racket2nix;
              # racket-fmt = final.deps.racket2nix.buildRacketPackage inputs.racket-fmt;
            };
            pkgs = (builtins.mapAttrs (name: pkg: appendversion pkg) pkgs-versioned) // pkgs-other;
          in
            pkgs // {pkgs = builtins.mapAttrs (name: _: final."${name}") pkgs;}
        );

      export = rec {
        packages = default.pkgs;

        # devShells.rust = default.rust-devshell;
        # devShells.cargo = default.test-cargo;

        legacyPackages = {
          homeConfigurations."alan" = default.home;

          inputs = inputs;

          p = default.legacypkgs;
          d = default.deps;

          pypkgs = packages.python.pkgs;

          _self = outputs-system;
          _mods = outputs-system.default.__mods;

          py310 = default (final: prev: {
            deps =
              prev.deps
              // {
                python = prev.legacypkgs.python310;
              };
          });

          emacs29-gtk3 = default (final: prev: {
            deps =
              prev.deps
              // {
                emacs = prev.legacypkgs.emacs29-gtk3;
              };
          });
        };
      };
    };
    forsystem = system: let
      outputs-system = forsystem-impl system outputs-system;
    in
      outputs-system;
    all = flake-utils.lib.eachDefaultSystem (
      system: (forsystem system).export
    );
  in
    all;
}
