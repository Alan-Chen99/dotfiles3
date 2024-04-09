{
  self,
  crane,
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
  coreutils,
}: let
  inherit (self) craneLib craneLib-dbg;
in rec {
  export.craneLib = (crane.mkLib legacypkgs).overrideToolchain rust;

  export.craneLib-dbg = craneLib.overrideScope (final: prev: {
    stdenv = std.keepDebugInfo std.stdenv;
  });

  # commonArgs = {
  #   src = nix-filter {
  #     root = src;
  #     include = [
  #       "src"
  #       "proc_macros"
  #       "Cargo.toml"
  #       "Cargo.lock"
  #     ];
  #   };
  #   OPENSSL_LIB_DIR = "${openssl.out}/lib";
  #   OPENSSL_INCLUDE_DIR = "${openssl.dev}/include";
  #   LIBCLANG_PATH = "${llvm.libclang.lib}/lib";

  #   strictDeps = true;
  #   CARGO_PROFILE = "dev";
  # };

  # NIX_LIB_DIR = "${nix}/lib";

  rust-src = rust.passthru.availableComponents.rust-src;

  export.rust-src-deps = craneLib.vendorMultipleCargoDeps {
    cargoLockList = [
      ./rust-src-hack/Cargo.lock
      "${rust-src}/lib/rustlib/src/rust/Cargo.lock"
    ];
  };

  # export.cargo-artifacts = craneLib.buildDepsOnly commonArgs;

  # testwrapcargo = std.writeScriptBin "cargo" ''
  #   #! ${std.stdenv.shell}
  #   exec ${rust}/bin/cargo --manifest-path ${commonArgs.src}/Cargo.toml --target-dir /build/target "$@"
  # '';

  # export.rust-docs = cargo-artifacts;

  # craneLib-dbg.cargoDoc (commonArgs
  # // {
  #   inherit cargo-artifacts;
  # });

  # export.rust-packages = craneLib-dbg.buildPackage (commonArgs
  #   // {
  #     inherit cargo-artifacts;
  #     # nativeBuildInputs = [tree which];
  #     unpackPhase = ''
  #       echo "source at: ${commonArgs.src}"
  #       export CARGO_TARGET_DIR=/build/target
  #     '';
  #     buildPhaseCargoCommand = ''
  #       cd ${commonArgs.src}
  #       # tree -a /build
  #       cargoBuildLog=$(mktemp /build/cargoBuildLogXXXX.json)
  #       cargoWithProfile build --message-format json-render-diagnostics --locked >"$cargoBuildLog"
  #     '';
  #     doNotRemoveReferencesToVendorDir = true;
  #   });

  # https://github.com/rust-lang/rust/issues/95736
  export.rust-src-hack =
    std.runCommandLocal "rust-src-hack" {
      nativeBuildInputs = [rust];
    }
    ''
      export dir=$out
      mkdir $dir
      cp -r ${rust-src}/lib/rustlib/src/rust/library $dir/library
      cp ${./rust-src-hack/rust-src-workspace.toml} $dir/Cargo.toml
      cp ${rust-src}/lib/rustlib/src/rust/Cargo.lock $dir/Cargo.lock
      chmod +w $dir/Cargo.lock
      mkdir $dir/.cargo
      cp ${self.rust-src-deps}/config.toml $dir/.cargo/config.toml

      cd $dir/library/std
      cargo metadata --format-version 1 --offline >/dev/null
    '';

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
