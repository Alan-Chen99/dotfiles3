{
  self,
  coreutils,
  flakes,
  legacypkgs,
  lib,
  llvmpkgs,
  nix,
  src,
  std,
  system,
  tree,
  which,
}: {
  export.packages = {
    # dafny = self.dafny;
  };

  export.test = derivation {
    name = "test-drv";
    builder = std.stdenv.shell;
    system = system;
    args = [
      "-c"
      ''
        ${coreutils}/bin/printenv
        ${tree}/bin/tree -a /build
        # ${coreutils}/bin/ls -la /nix/store

        echo "hello" >> $out
        echo "${std.stdenv.shell}" >> $dev
      ''
    ];
    outputs = ["out" "dev"];
    # requiredSystemFeatures = ["recursive-nix"];
    # NIX_REMOTE_RECURSIVE_PROTOCOL_VERSION = "0x101";
  };
}
