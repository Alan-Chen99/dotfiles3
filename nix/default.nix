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
      dream2nix = flakes.dream2nix;
      gitignore-lib = flakes.gitignore.lib;
      home-manager = flakes.home-manager;
      nix = flakes.nix.packages.${system}.default;
      nix-filter = flakes.nix-filter;
      nixd = flakes.nixd.packages.${system}.default;
      nixpkgs-unstable = flakes.nixpkgs-unstable;
      poetry2nix = flakes.poetry2nix;
      rust-overlay = flakes.rust-overlay.overlays.default;
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
          cleansrc
          dbg
          deps
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
    inherit (prev) pdf-tools-epdfinfo;
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
    inherit (prev) js;
  }));

  mod.nixtools = callpackage ./nixtools.nix {} (reexport (prev: {
    inherit (prev) nixtools;
  }));

  mod.printing-overlay = callpackage ./printing-overlay.nix {};

  mod.profile = callpackage ./profile.nix {} (reexport (prev: {
    inherit (prev) profile;
  }));

  mod.profile-root = callpackage ./profile-root.nix {} (reexport (prev: {
    inherit (prev) profile-root;
  }));

  mod.python = callpackage ../python {} (reexport (prev: {
    inherit (prev) pythontools;
  }));

  mod.repl = callpackage ./repl.nix {} (reexport (prev: {
    inherit (prev) nixrepl-wrapper;
  }));

  mod.runcmd = callpackage ./command.nix {} (reexport (prev: {
    inherit (prev) run-cmd;
  }));

  mod.rust = callpackage ../rust {} (reexport (prev: {
    inherit (prev) rust-src-hack;
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
