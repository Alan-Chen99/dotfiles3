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

  jspkg = name:
    std.buildEnv {
      name = name;
      paths = [
        (pkgs.mkYarnPackage {
          nodejs = nodejs;
          src = cleansrc ./.;
          publishBinsFor = [
            name
          ];
          doDist = false;
        })
      ];
      pathsToLink = ["/bin"];
    };

  export.pyright = jspkg "pyright";
  export.basedpyright = jspkg "basedpyright";
  export.scmindent = jspkg "scmindent";
  export.prettier = jspkg "prettier";

  deps = pkgs.mkYarnPackage {
    nodejs = nodejs;

    src = cleansrc ./.;

    publishBinsFor = [
      "@fsouza/prettierd"
      "prettier"
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
