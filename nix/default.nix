{
  self,
  flakes,
  system,
}: rec {
  checkedjoin = x: y: assert builtins.length (builtins.attrNames (builtins.intersectAttrs x y)) == 0; x // y;
  export = checkedjoin pub mod-reexports;
  mod-reexports = builtins.foldl' checkedjoin {} (builtins.catAttrs "reexport" (builtins.attrValues mod));

  pub.__mods = mod;

  nixpkgs-flakes = flakes.nixpkgs;

  mod.package = import ./package-bootstrap.nix {
    inherit (self) dbg;
  };

  reexport = mod.package.reexport-with self;

  mod.deps =
    mod.package.import-package ./deps.nix {
      inherit system nixpkgs-flakes;
      crane = flakes.crane;
      crate2nix = flakes.crate2nix;
      dbg = self.dbg;
      dream2nix = flakes.dream2nix;
      emacs-overlay = flakes.emacs-overlay;
      gitignore-lib = flakes.gitignore.lib;
      home-manager = flakes.home-manager;
      mini-compile-commands = flakes.mini-compile-commands;
      nix = flakes.nix.packages.${system}.default;
      nix-filter = flakes.nix-filter;
      nix-ros-overlay = flakes.nix-ros-overlay.overlays.default;
      nixd = flakes.nixd.packages.${system}.default;
      nixpkgs-unstable = flakes.nixpkgs-unstable;
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
          home-manager-bin-wrapped
          legacypkgs
          lib
          nixconf-file
          nixmeta
          nixrepl-wrapper
          nixwrapper
          pdf-tools-epdfinfo
          pkgs-unstable
          profile
          run-cmd
          source-ver
          src
          std
          ;
        flakes-self = flakes.self;
      };
  in
    mod.package.call-package-with aliases;

  mod.cxx = callpackage ./cxx.nix {} (reexport (prev: {
    inherit (prev) cxxtools;
  }));

  mod.debug = callpackage ./debug.nix {};
  pub.dbg = mod.debug;

  mod.emacs = callpackage ../emacs {} (reexport (prev: {
    inherit (prev) pdf-tools-epdfinfo emacs emacs-test;
  }));

  mod.env = callpackage ./env.nix {} (reexport (prev: {
    inherit (prev) nixwrapper flake-registry-file nixconf-file;
  }));

  mod.fonts = callpackage ./fonts.nix {} (reexport (prev: {
    inherit (prev) fonts;
  }));

  mod.home = callpackage ./home.nix {} (reexport (prev: {
    inherit (prev) home home-manager-bin-wrapped;
  }));

  mod.js = callpackage ../js {} (reexport (prev: {
    inherit (prev) js scmindent basedpyright;
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
      basedpyright-wrapped
      env-scripts
      poetrypython
      pypkgs-bins
      python-all
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
    inherit (prev) rust-src-hack craneLib;
  }));

  mod.source = callpackage ./source.nix {} (reexport (prev: {
    inherit (prev) cleansrc src;
  }));

  mod.test = callpackage ./test.nix {} (reexport (prev: {
    inherit (prev) test test2;
  }));

  mod.version = callpackage ./version.nix {} (reexport (prev: {
    inherit (prev) source-ver version nixmeta;
  }));

  pub.nixd-eval = builtins.foldl' builtins.seq true (builtins.map (pkg: pkg.nixd-eval or null) (builtins.attrValues mod) ++ [true]);
}
