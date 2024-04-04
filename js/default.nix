{
  self,
  legacypkgs,
  nix-filter,
  nodejs,
  src,
  std,
}: rec {
  # legacyPackages.x86_64-linux._mods.js.x

  pkgs = legacypkgs;

  corepack = pkgs.corepack.override {nodejs = nodejs;};

  deps = pkgs.mkYarnPackage {
    nodejs = nodejs;

    src = "${src}/js";

    publishBinsFor = [
      "@fsouza/prettierd"
      "typescript"
      "typescript-language-server"
    ];
  };

  export.js = std.buildEnv {
    name = "js";
    paths = [
      nodejs
      corepack
      deps
    ];
  };
}
