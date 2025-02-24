{
  self,
  flakes,
  crane,
  crate2nix,
  dbg,
  dream2nix,
  emacs-overlay,
  gitignore-lib,
  mini-compile-commands,
  nix-filter,
  nixpkgs-flakes,
  nixpkgs-unstable,
  poetry2nix,
  racket2nix,
  rust-overlay,
  system,
}: rec {
  # export.legacypkgs = nixpkgs-flakes.legacyPackages.${system};
  export.legacypkgs = import nixpkgs-flakes {
    inherit system;
    config = {
      allowUnfree = true;
      cudaSupport = true;
    };
    overlays = [
      rust-overlay
      flakes.nix-ros-overlay.overlays.default
      # emacs-overlay.overlays.emacs
    ];
  };

  export.pkgs-unstable = nixpkgs-unstable.legacyPackages."${system}";

  export.lib = self.legacypkgs.lib;

  export.std = std_ // dbg.__clean;
  std_ = {
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
      dbg
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
      dejavu_fonts
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
      nerdfonts
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
      crate2nix
      dream2nix
      gitignore-lib
      # home-manager
      nix-filter
      poetry2nix
      racket2nix
      ;

    nix = flakes.nix.packages.${system}.default.overrideAttrs {allowSubstitutes = true;};
    nix-stable = pkgs.nix;
    nixd = flakes.nixd.packages.${system}.default;
    nix-gl = flakes.nix-gl.packages.${system}.default;

    mcc-env = (pkgs.callPackage mini-compile-commands {}).wrap self.std.stdenv;
    mcc-hook = (pkgs.callPackage mini-compile-commands {}).hook;

    home-manager = flakes.home-manager;
    home-manager-bin = self.deps.home-manager.packages.${system}.home-manager;
    coreutils = pkgs.coreutils-full;
    emacs-base = pkgs.emacs30-pgtk;

    gcc = pkgs.gcc_latest;

    llvmpkgs = pkgs.llvmPackages_18;
    clangtools = self.deps.llvmpkgs.clang-tools;

    # TODO (1/22/2025): nodejs_latest is not in binary cache
    # nodejs = pkgs.nodejs_latest;
    nodejs = pkgs.nodejs_22;

    python = pkgs.python312.override {
      # enableOptimizations = true;
      # reproducibleBuild = false;
      self = self.deps.python;
    };

    rust = pkgs.rust-bin.nightly."2025-01-25".default.override {
      extensions = [
        "clippy"
        "cargo"
        "rustfmt-preview"
        "rust-analyzer"
      ];
    };
    # craneLib = (crane.mkLib pkgs).overrideToolchain rust;
  };
}
