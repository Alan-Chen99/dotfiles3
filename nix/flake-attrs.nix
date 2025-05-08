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

      p = attrs.legacypkgs;
      d = attrs.deps;
      pypkgs = attrs.poetrypython.python.pkgs;

      homeConfigurations."alan" = attrs.home;

      inputs = inputs_;

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

            nix-stable = final.deps.nix-stable;
            python = final.poetrypython.python;
          };
        in {
          packages = prev.packages // pkgs // final.pypkgs-bins // final.experimental.packages;
        }
      );

      with-extend-pkgs = add-overlay (
        final: prev:
          extend-pkgs
          // {
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
          pkgs = builtins.mapAttrs (name: pkg: appendversion pkg) pkgs-versioned;
        in
          pkgs
          // {
            packages = prev.packages // pkgs;
          }
      );

      extend-pkgs.emacs29 = with-emacs29.emacs;
      with-emacs29 = add-deps-overlay (final: prev: {
        emacs-base = prev.deps.emacs-base.overrideAttrs {
          version = "29.4.50";
          src = inputs.emacs29;
        };
      });

      extend-pkgs.emacs31 = with-emacs31.emacs;
      with-emacs31 = add-deps-overlay (final: prev: {
        emacs-base = prev.deps.emacs-base.overrideAttrs {
          version = "31.0.50";
          src = inputs.emacs31;
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
            nix = final.deps.nix-stable;
          };
      });
    };
  in
    attrs // res;
in
  base-init.default-pkgs.with-extend-pkgs.set-versions
