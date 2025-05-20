{
  description = "configs";
  # nix --experimental-features "nix-command flakes" --allow-import-from-derivation build .#profile -v --print-build-logs

  # cachix use cuda-maintainers

  # nix run p#nix-stable -- flake metadata

  inputs = {
    # newadwaita-slim = {
    #   # url = "git+https://github.com/ManFridayy/NewAdwaita-slim.git";
    #   url = "https://github.com/ManFridayy/NewAdwaita-slim/releases/download/0.1/NewAdwaita-slim.tar.xz";
    #   flake = false;
    # };

    # yaru = {
    #   url = "git+file:///home/alan/yaru";
    #   flake = false;
    # };

    crane = {
      url = "github:ipetkov/crane";
    };

    crate2nix = {
      url = "github:nix-community/crate2nix";
      flake = false;
    };

    dafny = {
      # url = "github:Alan-Chen99/dafny";
      url = "github:dafny-lang/dafny/4.10.0";
      flake = false;
    };

    dream2nix = {
      url = "github:nix-community/dream2nix";
      flake = false;
    };

    emacs30 = {
      url = "github:emacs-mirror/emacs/emacs-30.1";
      flake = false;
    };

    emacs31 = {
      url = "github:emacs-mirror/emacs/master";
      flake = false;
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
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
      # https://github.com/NixOS/nix/pull/6530
      # https://github.com/edolstra/nix/tree/lazy-trees
      url = "github:edolstra/nix/lazy-trees";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-23-11.follows = "empty";
        nixpkgs-regression.follows = "empty";
        flake-compat.follows = "flake-compat";
      };
    };

    nix-filter.url = "github:numtide/nix-filter";

    nix-gl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    nix-index = {
      url = "github:nix-community/nix-index";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-ros-overlay = {
      url = "github:lopsided98/nix-ros-overlay/master";
      # inputs.nixpkgs.follows = "empty";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    nixd = {
      url = "github:nix-community/nixd/release/1.2";
      inputs.nixpkgs.follows = "nixpkgs23-11";
      inputs.flake-parts.follows = "flake-parts";
    };

    nixpkgs = {
      # nix flake update nixpkgs
      url = "github:nixos/nixpkgs/nixos-24.11";
      # url = "github:nixos/nixpkgs/ae584d90cbd0396a422289ee3efb1f1c9d141dc3";
    };
    # nixpkgs-lib.url = "github:nixos/nixpkgs/ae584d90cbd0396a422289ee3efb1f1c9d141dc3?dir=lib";
    nixpkgs-lib.url = "github:nix-community/nixpkgs.lib";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nixpkgs21-11.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs22-11.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs23-11.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs24-05.url = "github:nixos/nixpkgs/nixos-24.05";

    poetry2nix = {
      url = "github:Alan-Chen99/poetry2nix";
      # url = "git+file:///home/alan/poetry2nix";
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
        # flake-utils.follows = "flake-utils";
      };
    };

    schemat = {
      url = "github:raviqqe/schemat";
      flake = false;
    };

    systems.follows = "flake-utils/systems";

    z3 = {
      url = "github:Z3Prover/z3/z3-4.12.1";
      flake = false;
    };
  };

  outputs = inputs @ {
    self,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: rec {
        legacyPackages = import ./nix/flake-attrs.nix {
          inputs = inputs;
          system = system;
        };
        packages = legacyPackages.packages;
      }
    );
}
