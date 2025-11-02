{
  inputs,
  system,
}: let
  package = import ./package-bootstrap.nix {};

  attrs-init =
    package.import-package ./default.nix {
      flakes = inputs;
      system = system;
    } (final: prev: {
      packages = {};
    });

  base-init = f attrs-init;

  inputs_ = inputs;

  f = attrs: let
    res = rec {
      base = base-init;
      default = attrs;

      n = attrs.legacypkgs;
      p = attrs.legacypkgs;
      d = attrs.deps;

      inputs = inputs_;

      inherit
        (attrs.deps)
        past-24-11-1
        past-24-11-2
        update-python
        ;

      add-overlay = overlay: (f (attrs overlay));

      add-deps-overlay = overlay: (add-overlay (
        final: prev: {
          deps = prev.deps // (overlay final prev);
        }
      ));

      default-pkgs = add-overlay (
        final: prev: let
          pkgs = {
            inherit
              (final)
              emacs
              nixwrapper
              pdf-tools-epdfinfo
              pypkgs-all
              yaru-theme
              ;

            nix-stable = final.deps.nix-stable;
            python = final.python-inject;

            # nix-index = inputs.nix-index.packages.${system}.default;
            nix-index-database = inputs.nix-index-database.packages.${system}.default;

            home-manager = final.deps.home-manager-bin;
          };
        in {
          # impure
          nix-gl = inputs.nix-gl.packages.${system}.default;

          packages =
            prev.packages
            // pkgs
            // final.jspkgs-bins
            // final.pypkgs-bins
            // final.rustpkgs-bins
            // final.experimental.packages;
        }
      );

      with-extend-pkgs = add-overlay (
        final: prev: {
          packages = prev.packages // extend-pkgs;
        }
      );

      set-versions = add-overlay (
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
              # env-scripts
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
          pkgs = builtins.mapAttrs (name: pkg: appendversion pkg) pkgs-versioned;
        in
          pkgs
          // {
            packages = prev.packages // pkgs;
          }
      );

      with-unstable = add-overlay (final: prev: {
        nixpkgs-src = inputs.nixpkgs-unstable;
      });

      homeConfigurations."alan" = attrs.home;
      # for github actions
      homeConfigurations."runner" = with-username-runner.home;
      with-username-runner = add-overlay (final: prev: {
        user-name = "runner";
      });

      extend-pkgs.emacs29 = with-emacs29.emacs;
      with-emacs29 = add-deps-overlay (final: prev: {
        emacs-base = past-24-11-2.packages.emacs29;
      });

      extend-pkgs.emacs30 = with-emacs30.emacs;
      with-emacs30 = add-deps-overlay (final: prev: {
        emacs-base = final.legacypkgs.emacs30-pgtk.overrideAttrs {
          version = "30.1";
          src = inputs.emacs30;
        };
      });

      extend-pkgs.emacs-gtk = with-emacs-gtk.emacs;
      with-emacs-gtk = add-deps-overlay (final: prev: {
        emacs-base = prev.deps.emacs-base.override {
          withPgtk = false;
          withGTK3 = true;
        };
      });

      with-py310 = add-deps-overlay (final: prev: {
        python = final.legacypkgs.python310;
      });

      less-download-flakes = add-overlay (final: prev: {
        less-download-flakes = true;
      });

      less-build = add-overlay (final: prev: {
        emacs = final.deps.emacs-base;
        deps =
          prev.deps
          // {
            emacs-base = final.legacypkgs.emacs30-gtk3;
            # nix = final.deps.nix-stable;
          };
      });

      with-nix-stable = add-deps-overlay (final: prev: {
        nix = final.deps.nix-stable;
      });
    };
  in
    attrs // res;
in
  base-init.default-pkgs.with-extend-pkgs.set-versions
