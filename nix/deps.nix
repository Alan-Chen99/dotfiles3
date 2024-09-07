{
  self,
  crane,
  crate2nix,
  dream2nix,
  emacs-overlay,
  gitignore-lib,
  home-manager,
  mini-compile-commands,
  nix,
  nix-filter,
  nix-ros-overlay,
  nixd,
  nixpkgs-flakes,
  nixpkgs-unstable,
  poetry2nix,
  racket2nix,
  rust-overlay,
  system,
}: {
  # export.legacypkgs = nixpkgs-flakes.legacyPackages.${system};
  export.legacypkgs = import nixpkgs-flakes {
    inherit system;
    overlays = [
      (final: prev: {
        # nix = final.nixVersions.nix_2_18;
        nix-stable_ = prev.nix;
        nix = nix;
      })
      rust-overlay
      nix-ros-overlay
      # emacs-overlay.overlays.emacs
    ];
  };

  export.pkgs-unstable = nixpkgs-unstable.legacyPackages."${system}";

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
      nix
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
      home-manager
      nix-filter
      nixd
      poetry2nix
      racket2nix
      ;

    mcc-env = (pkgs.callPackage mini-compile-commands {}).wrap self.std.stdenv;
    mcc-hook = (pkgs.callPackage mini-compile-commands {}).hook;

    home-manager-bin = pkgs.home-manager;
    coreutils = pkgs.coreutils-full;
    emacs-base = pkgs.emacs29-pgtk;

    gcc = pkgs.gcc_latest;

    llvmpkgs = pkgs.llvmPackages_17;
    clangtools = (pkgs.clang-tools).override {
      llvmPackages = self.deps.llvmpkgs;
      # enableLibcxx = true;
    };

    nodejs = pkgs.nodejs_latest;

    nix_2_16 = pkgs.nixVersions.nix_2_16;
    nix-stable = pkgs.nix-stable_;

    python = pkgs.python312.override {
      # enableOptimizations = true;
      # reproducibleBuild = false;
      self = self.deps.python;
    };

    rust = pkgs.rust-bin.nightly."2023-10-31".default.override {
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
