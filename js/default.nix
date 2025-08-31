{
  self,
  cleansrc,
  flakes,
  legacypkgs,
  lib,
  nix-filter,
  nodejs,
  std,
}: rec {
  # legacyPackages.x86_64-linux._mods.js.x

  pkgs = legacypkgs;

  corepack = pkgs.corepack.override {nodejs = nodejs;};

  postInstall = ''
    wrapProgram $out/bin/yarn2nix --prefix PATH : "${pkgs.nix-prefetch-git}/bin"
  '';

  jspkg = {
    name,
    publishBinsFor ? [name],
    ...
  } @ attrs:
    std.buildEnv {
      name = name;
      paths = [
        (pkgs.mkYarnPackage ({
            nodejs = nodejs;
            src = cleansrc ./.;
            publishBinsFor = publishBinsFor;
            doDist = false;
          }
          // (builtins.removeAttrs attrs ["name" "publishBinsFor"])))
      ];
      pathsToLink = ["/bin"];
    };

  export.basedpyright = jspkg {name = "basedpyright";};
  export.pyright = jspkg {name = "pyright";};

  prettier = jspkg {
    name = "prettier";
    publishBinsFor = ["prettier" "@fsouza/prettierd"];
    postInstall = ''
      wrapProgram $out/bin/prettierd --prefix PATH : "${nodejs}/bin"
    '';
    nativeBuildInputs = [pkgs.makeWrapper];
  };

  export.jspkgs-bins = {
    nvm = jspkg {name = "nvm";};
    password-generator = jspkg {name = "@sebastienrousseau/password-generator";};
    prettier = prettier;
    scmindent = jspkg {name = "scmindent";};
  };

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
