{
  self,
  flakes,
  system,
}: rec {
  checkedjoin = x: y: assert builtins.length (builtins.attrNames (builtins.intersectAttrs x y)) == 0; x // y;
  export = checkedjoin pub mod-reexports;
  mod-reexports = builtins.foldl' checkedjoin {} (builtins.catAttrs "reexport" (builtins.attrValues mod));

  pub.mods = mod;

  nixpkgs-flakes = flakes.nixpkgs;

  mod.package = import ./package-bootstrap.nix {
    inherit (self) dbg;
  };

  reexport = mod.package.reexport-with self;

  mod.deps =
    mod.package.import-package ./deps.nix {
      inherit system nixpkgs-flakes;
      flakes = flakes;
      crane = flakes.crane;
      dbg = self.dbg;
      gitignore-lib = flakes.gitignore.lib;
      mini-compile-commands = flakes.mini-compile-commands;
      nix-filter = flakes.nix-filter;
      # nixd = flakes.nixd.packages.${system}.default;
      poetry2nix = flakes.poetry2nix;
      rust-overlay = flakes.rust-overlay.overlays.default;
      racket2nix = import flakes.racket2nix {
        inherit system;
        pkgs = import "${flakes.racket2nix}/pkgs" {
          inherit system;
          # pkgs = flakes.nixpkgs22-11.legacyPackages."${system}";
          pkgs =
            import (builtins.fetchTarball (
              builtins.removeAttrs
              (builtins.fromJSON (builtins.readFile "${flakes.racket2nix}/nixpkgs/default.json"))
              ["unpack"]
            )) {
              inherit system;
            };
        };
      };
    } (final: prev: {
      lib-orig = prev.lib;
      lib = final.lib-orig.extend mod.printing-overlay.lib-overlay;
    }) (reexport (prev: {
      inherit (prev) legacypkgs lib lib-orig std deps pkgs-unstable;
    }));

  pub.callpackage = callpackage;
  callpackage = let
    aliases =
      (builtins.mapAttrs (name: _: self.deps."${name}") mod.deps.reexport.deps)
      // {
        inherit
          callpackage
          flakes
          nixpkgs-flakes
          system
          ;
        inherit
          (self)
          basedpyright
          cleansrc
          dbg
          deps
          emacs
          fonts
          legacypkgs
          lib
          nixconf-file
          nixmeta
          nixrepl-wrapper
          nixwrapper
          pdf-tools-epdfinfo
          pkgs-unstable
          profile
          pypkgs-bins
          run-cmd
          source-ver
          src
          std
          yaru-theme
          ;
        flakes-self = flakes.self;
        super = self;
      };
  in
    mod.package.call-package-with aliases;

  mod.ci = callpackage ./ci.nix {} (reexport (prev: {
    inherit (prev) ci-deps ci-instantiate;
  }));

  mod.cxx = callpackage ./cxx.nix {} (reexport (prev: {
    inherit (prev) cxxtools;
  }));

  mod.debug = callpackage ./debug.nix {};
  pub.dbg = mod.debug;

  mod.emacs = callpackage ../emacs {} (reexport (prev: {
    inherit (prev) pdf-tools-epdfinfo emacs;
  }));

  mod.env = callpackage ./env.nix {} (reexport (prev: {
    inherit (prev) nixwrapper flake-registry-file nixconf-file less-download-flakes;
  }));

  mod.experimental = callpackage ./experimental.nix {};
  pub.experimental = mod.experimental;

  mod.fonts = callpackage ./fonts.nix {} (reexport (prev: {
    inherit (prev) fonts;
  }));

  mod.home = callpackage ./home.nix {} (reexport (prev: {
    inherit (prev) home user-name;
  }));

  mod.js = callpackage ../js {} (reexport (prev: {
    inherit
      (prev)
      js
      jspkgs-bins
      #
      # basedpyright
      # pyright
      ;
  }));

  mod.latex = callpackage ./latex.nix {} (reexport (prev: {
    inherit (prev) latexenv;
  }));

  mod.misc = callpackage ./misc.nix {} (reexport (prev: {
    inherit (prev) yaru-theme iperf3;
  }));

  mod.nixtools = callpackage ./nixtools.nix {} (reexport (prev: {
    inherit (prev) nixtools;
  }));

  mod.printing-overlay = callpackage ./printing-overlay.nix {};

  mod.profile = callpackage ./profile.nix {} (reexport (prev: {
    inherit (prev) profile pkgs-small pkgs-big;
  }));

  mod.profile-root = callpackage ./profile-root.nix {} (reexport (prev: {
    inherit (prev) profile-root;
  }));

  mod.python = callpackage ../python {} (reexport (prev: {
    inherit
      (prev)
      pypkgs
      pypkgs-all
      pypkgs-bins
      python-inject
      pythonlibs
      pythontools
      ;
  }));

  mod.repl = callpackage ./repl.nix {} (reexport (prev: {
    inherit (prev) nixrepl-wrapper;
  }));

  mod.runcmd = callpackage ./command.nix {} (reexport (prev: {
    inherit (prev) run-cmd;
  }));

  mod.rust = callpackage ../rust {} (reexport (prev: {
    inherit (prev) rustpkgs-bins;
  }));

  mod.source = callpackage ./source.nix {} (reexport (prev: {
    inherit (prev) cleansrc src;
  }));

  mod.version = callpackage ./version.nix {} (reexport (prev: {
    inherit (prev) source-ver version nixmeta;
  }));

  pub.nixd-eval = builtins.foldl' builtins.seq true (builtins.map (pkg: pkg.nixd-eval or null) (builtins.attrValues mod) ++ [true]);
}
