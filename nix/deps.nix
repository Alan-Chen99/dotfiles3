{
  self,
  flakes,
  crane,
  dbg,
  gitignore-lib,
  mini-compile-commands,
  nix-filter,
  nixpkgs-flakes,
  poetry2nix,
  racket2nix,
  rust-overlay,
  system,
}: rec {
  export.nixpkgs-src = nixpkgs-flakes;
  export.nixpkgs-config = {
    inherit system;
    config = {
      allowUnfree = true;
      cudaSupport = true;
      permittedInsecurePackages = [
        "emacs-29.4.50"
      ];
    };
    overlays = [
      flakes.emacs-overlay.overlays.emacs
      flakes.nix-ros-overlay.overlays.default
      rust-overlay
    ];
  };

  export.legacypkgs = import self.nixpkgs-src self.nixpkgs-config;
  export.pkgs-unstable = import flakes.nixpkgs-unstable self.nixpkgs-config;

  export.lib = self.legacypkgs.lib;

  export.std = {
    inherit
      (self.legacypkgs)
      breakpointHook
      buildEnv
      keepDebugInfo
      makeWrapper
      mkShell
      mkShellNoCC
      runCommandLocal
      runCommandCC
      stdenv
      writeScriptBin
      writeShellScript
      writeShellScriptBin
      writeText
      writeTextDir
      ;

    mkDerivation = self.legacypkgs.stdenv.mkDerivation;
  };

  export.deps = let
    pkgs = self.legacypkgs;
  in {
    inherit
      (pkgs)
      alejandra
      bashInteractive
      buildFHSUserEnv
      cacert
      cachix
      diffstat
      diffutils
      direnv
      fd
      file
      findutils
      flamegraph
      gawk
      gdb
      git
      glibcLocales
      gnugrep
      gnused
      keychain
      nix-doc
      nix-plugins
      nix-tree
      openssl
      patch
      pyright
      ripgrep
      taplo
      tree
      tree-sitter
      tzdata
      which
      ;

    # flakes
    inherit
      crane
      gitignore-lib
      # home-manager
      nix-filter
      poetry2nix
      racket2nix
      ;

    # nix = flakes.nix.packages.${system}.default.overrideAttrs {allowSubstitutes = true;};
    nix = pkgs.nixVersions.nix_2_28;
    nix-stable = pkgs.nixVersions.stable;
    nixd = flakes.nixd.packages.${system}.default;

    mcc-env = (pkgs.callPackage mini-compile-commands {}).wrap self.std.stdenv;
    mcc-hook = (pkgs.callPackage mini-compile-commands {}).hook;

    home-manager = flakes.home-manager;
    home-manager-bin = self.deps.home-manager.packages.${system}.home-manager;
    coreutils = pkgs.coreutils-full;

    emacs-base = pkgs.emacs-git-pgtk.overrideAttrs {
      version = "31.0.50";
      src = flakes.emacs31;
    };

    gcc = pkgs.gcc_latest;

    llvmpkgs = pkgs.llvmPackages_18;
    clangtools = self.deps.llvmpkgs.clang-tools;

    nodejs = pkgs.nodejs_latest;

    python = pkgs.python312.override {
      # enableOptimizations = true;
      # reproducibleBuild = false;
      self = self.deps.python;
    };

    rust = pkgs.rust-bin.nightly.latest.default.override {
      extensions = [
        "clippy"
        "cargo"
        "rustfmt-preview"
        "rust-analyzer"
      ];
      targets = ["x86_64-unknown-linux-musl"];
    };

    texlive = pkgs.texliveSmall;

    past-24-11-1 = import (flakes.past-24-11-1 + "/nix/local.nix") {system = system;};

    update-python = import flakes.update-python {system = system;};
  };
}
