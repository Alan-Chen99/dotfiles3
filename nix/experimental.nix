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
    dafny = self.dafny;
  };

  # https://nixos.wiki/wiki/DotNET
  # on update:
  # (1) nix build .#dafny.fetch-deps
  # (2) edit out localtion to dafny_deps.json
  export.dafny = (
    (legacypkgs.dafny.override (prev: {
      buildDotnetModule = args:
        prev.buildDotnetModule (args
          // {
            nugetDeps = ./dafny_deps.json;
            dotnet-runtime = legacypkgs.dotnet-sdk;
          });
      dafny = self.dafny;
      z3 = legacypkgs.z3_4_12.overrideAttrs {
        src = flakes.z3;
        version = "4.12.1";
      };
    }))
    .overrideAttrs {
      src = flakes.dafny;
      version = "4.10.0";
    }
  );

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
