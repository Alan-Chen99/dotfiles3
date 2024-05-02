{
  self,
  legacypkgs,
  nix-filter,
  nodejs,
  cleansrc,
  std,
}: rec {
  # legacyPackages.x86_64-linux._mods.js.x

  pkgs = legacypkgs;

  corepack = pkgs.corepack.override {nodejs = nodejs;};

  export.scmindent = std.buildEnv {
    name = "scmindent";
    paths = [
      (pkgs.mkYarnPackage {
        nodejs = nodejs;
        src = cleansrc ./.;
        publishBinsFor = [
          "scmindent"
        ];
        doDist = false;
      })
    ];
    pathsToLink = ["/bin"];
  };

  deps = pkgs.mkYarnPackage {
    nodejs = nodejs;

    src = cleansrc ./.;

    publishBinsFor = [
      "@fsouza/prettierd"
      "typescript"
      "typescript-language-server"
    ];

    doDist = false;
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
