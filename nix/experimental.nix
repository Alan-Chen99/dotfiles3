{
  self,
  flakes,
  legacypkgs,
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
}
