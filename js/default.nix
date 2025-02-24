{
  self,
  cleansrc,
  flakes,
  legacypkgs,
  nix-filter,
  nodejs,
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
      "eslint"
      "eslint-lsp"
      "prettier"
      "ts-node"
      "tsx"
      "typescript"
      "typescript-language-server"
    ];

    doDist = false;
  };

  # pkgs_ = pkgs.extend (final: prev: {
  #   nodejs = nodejs;
  # });
  # export.basedpyright = pkgs_.basedpyright.overrideAttrs (final: prev: {
  #   version = flakes.basedpyright.shortRev;
  #   src = flakes.basedpyright;
  #   npmDepsHash = "sha256-4yc53xonguaPIem5/iWDw1g9D4DwuIBOTTny0UmhPB0=";
  #   npmDeps = pkgs.fetchNpmDeps {
  #     inherit (final) src;
  #     name = "${final.pname}-${final.version}-npm-deps";
  #     hash = final.npmDepsHash;
  #   };
  # });

  export.js = std.buildEnv {
    name = "js";
    paths = [
      nodejs
      corepack
      deps
    ];
  };
}
