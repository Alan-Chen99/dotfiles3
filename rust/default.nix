{
  self,
  coreutils,
  crane,
  flakes,
  legacypkgs,
  lib,
  llvmpkgs,
  nix,
  nix-filter,
  openssl,
  run-cmd,
  rust,
  src,
  std,
  taplo,
  tree,
  which,
  dbg,
}: let
  inherit (self) craneLib craneLib-dbg;
  pkgs = legacypkgs;
in rec {
  # craneLib = (inputs.crane.mkLib pkgs).overrideScope (final: prev: {
  #     # We override the behavior of `mkCargoDerivation` by adding a wrapper which
  #     # will set a default value of `CARGO_PROFILE` when not set by the caller.
  #     # This change will automatically be propagated to any other functions built
  #     # on top of it (like `buildPackage`, `cargoBuild`, etc.)
  #     mkCargoDerivation = args: prev.mkCargoDerivation ({
  #       CARGO_PROFILE = "bench"; # E.g. always build in benchmark mode unless overridden
  #     } // args);
  #   });

  export.craneLib = (crane.mkLib pkgs).overrideToolchain rust;

  # this tries to solve a fetch fail but it seems to be only a cache bug in nix?
  # does nothing, and then
  # a manual call to fetchGit in a repl worked without allRefs
  # TODO: look at narinfo-cache-negative-ttl=0
  # craneLib_ = (crane.mkLib pkgs).overrideToolchain rust;
  # export.craneLib = craneLib_.overrideScope (final: prev: {
  #   downloadCargoPackageFromGit = args: let
  #     ans = prev.downloadCargoPackageFromGit (
  #       builtins.seq (dbg.pr args) (args // {allRefs = true;})
  #     );
  #   in
  #     builtins.seq (dbg.pr ans) ans;
  # });

  export.craneLib-dbg = craneLib.overrideScope (final: prev: {
    stdenv = std.keepDebugInfo std.stdenv;
  });

  export.rustpkgs-bins.schemat = craneLib.buildPackage {
    src = flakes.schemat;
    strictDeps = true;
  };

  export.rustpkgs-bins.uv = let
    src = flakes.uv;

    rust = pkgs.rust-bin.fromRustupToolchainFile "${src}/rust-toolchain.toml";
    craneLib = (crane.mkLib pkgs).overrideToolchain rust;

    name = craneLib.crateNameFromCargoToml {src = "${src}/crates/uv";};
  in
    craneLib.buildPackage {
      inherit (name) pname version;
      src = src;
      strictDeps = true;
      doCheck = false;
    };

  export.rustpkgs-bins.uv-upgrade = let
    src = flakes.uv-upgrade;

    rust = pkgs.rust-bin.fromRustupToolchainFile "${src}/rust-toolchain.toml";
    craneLib = (crane.mkLib pkgs).overrideToolchain rust;

    name = craneLib.crateNameFromCargoToml {src = "${src}/crates/uv";};
  in
    craneLib.buildPackage {
      inherit (name) pname version;
      src = src;
      strictDeps = true;
      doCheck = false;

      stdenv = p: p.clangStdenv;

      hardeningDisable = ["format"];

      LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
    };

  commonArgs = {
    src = nix-filter {
      root = src + "/rust";
      include = [
        "src"
        "Cargo.toml"
        "Cargo.lock"
        ".cargo"
      ];
    };
    # OPENSSL_LIB_DIR = "${openssl.out}/lib";
    # OPENSSL_INCLUDE_DIR = "${openssl.dev}/include";
    # LIBCLANG_PATH = "${llvm.libclang.lib}/lib";

    strictDeps = true;
    # CARGO_PROFILE = "dev";
  };

  export.cargoArtifacts = craneLib.buildDepsOnly commonArgs;
  export.cargoArtifacts-static = craneLib.buildDepsOnly (commonArgs
    // {
      CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
    });

  export.test_script = craneLib.buildPackage (commonArgs
    // {
      cargoArtifacts = self.cargoArtifacts;

      cargoExtraArgs = "--bin test_script";
    });

  export.setuid = craneLib.buildPackage (commonArgs
    // {
      cargoArtifacts = self.cargoArtifacts-static;

      cargoExtraArgs = "--bin setuid";
      CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
    });

  # testwrapcargo = std.writeScriptBin "cargo" ''
  #   #! ${std.stdenv.shell}
  #   exec ${rust}/bin/cargo --manifest-path ${commonArgs.src}/Cargo.toml --target-dir /build/target "$@"
  # '';

  # export.rust-docs = cargo-artifacts;

  # craneLib-dbg.cargoDoc (commonArgs
  # // {
  #   inherit cargo-artifacts;
  # });

  export.rust-packages = craneLib.buildPackage (commonArgs
    // {
      inherit (self) cargo-artifacts;
      # nativeBuildInputs = [tree which];
      unpackPhase = ''
        echo "source at: ${commonArgs.src}"
        export CARGO_TARGET_DIR=/build/target
      '';
      buildPhaseCargoCommand = ''
        cd ${commonArgs.src}
        # tree -a /build
        cargoBuildLog=$(mktemp /build/cargoBuildLogXXXX.json)
        cargoWithProfile build --message-format json-render-diagnostics --locked >"$cargoBuildLog"
      '';
      doNotRemoveReferencesToVendorDir = true;
    });

  # export.rust-devshell = std.mkShellNoCC (commonArgs
  #   // {
  #     inputsFrom = [
  #       self.rust-packages
  #     ];
  #     packages = [
  #       taplo
  #       # pkgs.cargo-audit
  #       # pkgs.cargo-watch
  #     ];
  #     # buildInputs = [
  #     #   nix.dev
  #     # ];
  #     RUST_SRC_PATH = "${self.rust-src-hack}/library";
  #     NIX_LIB_DIR = "${nix}/lib";
  #     NIX_INCLUDE_DIR = "${nix.dev}/include/nix";
  #   });

  # export.test-cargo = std.mkShellNoCC (commonArgs
  #   // {
  #     buildInputs = [
  #       legacypkgs.file
  #       legacypkgs.curl
  #       legacypkgs.python3
  #       legacypkgs.openssl
  #       legacypkgs.zlib
  #     ];
  #     packages = [
  #       taplo
  #       rust
  #     ];
  #     RUST_SRC_PATH = "${self.rust-src-hack}/library";
  #   });
}
