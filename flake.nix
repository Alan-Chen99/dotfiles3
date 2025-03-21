{
  description = "configs";
  # nix --experimental-features "nix-command flakes" --allow-import-from-derivation build .#profile -v --print-build-logs

  # cachix use cuda-maintainers

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
    basedpyright = {
      url = "git+https://github.com/DetachHead/basedpyright.git";
      flake = false;
    };

    crane = {
      url = "github:ipetkov/crane";
    };

    crate2nix = {
      url = "github:nix-community/crate2nix";
      inputs.cachix.follows = "empty";
      inputs.crate2nix_stable.follows = "empty";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-test-runner.follows = "empty";
    };

    dafny = {
      url = "github:Alan-Chen99/dafny";
      flake = false;
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
    nixpkgs24-05.url = "github:nixos/nixpkgs/797f7dc49e0bc7fab4b57c021cdf68f595e47841";

    poetry2nix = {
      url = "git+https://github.com/Alan-Chen99/poetry2nix.git";
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
  };

  outputs = inputs @ {
    self,
    flake-utils,
    ...
  }: let
    get-default = system: let
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
                ci-deps
                ci-instantiate
                cxxtools
                env-scripts
                flake-registry-file
                fonts
                js
                latexenv
                nixtools
                pkgs-big
                pkgs-small
                profile
                profile-root
                pythonlibs
                pythontools
                ;
            };
            pkgs-other = {
              inherit
                (prev)
                basedpyright
                emacs
                nixwrapper
                password-generator
                pdf-tools-epdfinfo
                poetry
                prettier
                pyright
                python-all
                scmindent
                test
                test2
                yaru-theme
                ;

              nix-stable = prev.deps.nix-stable;
              python = prev.poetrypython.python;

              # https://nixos.wiki/wiki/DotNET
              # on update:
              # (1) nix build .#dafny.fetch-deps
              # (2) edit out localtion to dafny_deps.json
              dafny =
                (final.legacypkgs.dafny.override (prev_: {
                  buildDotnetModule = args:
                    prev_.buildDotnetModule (args
                      // {
                        nugetDeps = ./dafny_deps.json;
                        dotnet-runtime = final.legacypkgs.dotnet-sdk;
                      });
                  dafny = final.dafny;
                }))
                .overrideAttrs (final_: prev_: {
                  src = inputs.dafny;
                  version = "4.9.1";
                  # for dafny 4.9.1
                  # postPatch before nixpkgs 4391342df7be (2/10/2025)
                  postPatch = let
                    # runtimeJarVersion = "4.9.1";
                    runtimeJarVersion = final_.version;
                  in ''
                    cp ${final.legacypkgs.writeScript "fake-gradlew-for-dafny" ''
                      mkdir -p build/libs/
                      javac $(find -name "*.java" | grep "^./src/main") -d classes
                      jar cf build/libs/DafnyRuntime-${runtimeJarVersion}.jar -C classes dafny
                    ''} Source/DafnyRuntime/DafnyRuntimeJava/gradlew

                    # Needed to fix
                    # "error NETSDK1129: The 'Publish' target is not supported without
                    # specifying a target framework. The current project targets multiple
                    # frameworks, you must specify the framework for the published
                    # application."
                    substituteInPlace Source/DafnyRuntime/DafnyRuntime.csproj \
                      --replace-fail TargetFrameworks TargetFramework \
                      --replace-fail "netstandard2.0;net452" net8.0

                    for f in Source/**/*.csproj ; do
                      [[ "$f" == "Source/DafnyRuntime/DafnyRuntime.csproj" ]] && continue;

                      substituteInPlace $f \
                        --replace-fail net6.0 net8.0
                    done
                  '';
                });

              # poetry2nix/tests/pyqt6/default.nix

              # rackt-test = final.deps.racket2nix;
              # racket-fmt = final.deps.racket2nix.buildRacketPackage inputs.racket-fmt;
            };
            pkgs = (builtins.mapAttrs (name: pkg: appendversion pkg) pkgs-versioned) // pkgs-other;
          in
            pkgs
            // {
              pkgs = final.pypkgs-bins // (builtins.mapAttrs (name: _: final."${name}") pkgs);
            }
        );
    in
      default;

    _inputs = inputs;

    from-default = default:
      default
      // {
        default = default;
      }
      // rec {
        packages = default.pkgs;

        homeConfigurations."alan" = default.home;

        inputs = _inputs;

        p = default.legacypkgs;
        d = default.deps;

        pypkgs = packages.python.pkgs;

        py310 = from-default (default (final: prev: {
          deps =
            prev.deps
            // {
              python = prev.legacypkgs.python310;
            };
        }));

        with_emacs-gtk = from-default (default (final: prev: {
          deps =
            prev.deps
            // {
              emacs-base = prev.legacypkgs.emacs30-gtk3;
            };
        }));
        emacs-gtk = with_emacs-gtk.emacs;

        less-download-flakes = from-default (default (final: prev: {
          less-download-flakes = true;
        }));

        less-build = from-default (default (final: prev: {
          emacs = prev.deps.emacs-base;
          deps =
            prev.deps
            // {
              nix = prev.deps.nix-stable;
            };
        }));

        nix-gl = default.deps.nix-gl;
      };

    forsystem = system: rec {
      legacyPackages = from-default (get-default system);
      packages = legacyPackages.packages;
    };
  in
    flake-utils.lib.eachDefaultSystem forsystem;
}
