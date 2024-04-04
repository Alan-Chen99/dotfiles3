{
  self,
  legacypkgs,
  poetry2nix,
  pyright,
  python,
  std,
}: rec {
  # pkgs = legacypkgs;
  # outputs.legacyPackages.x86_64-linux._self.default.__mods.python.__out

  poetry-lib = poetry2nix.lib.mkPoetry2Nix {pkgs = legacypkgs;};

  python-patched = python.override {
    self = python-patched;
    packageOverrides = final: prev: {
      pip = python-with-deps.pkgs.pip;
    };
  };

  python-with-deps = poetry-lib.mkPoetryEnv {
    python = python-patched;
    projectDir = ./.;
    preferWheels = true;
    overrides = [
      (final: prev: {
        cchardet = prev.cchardet.override {preferWheel = false;};

        # this is bc defaultPoetryOverrides says poetry = poetry_core
        _poetry = prev.poetry;
      })
      poetry-lib.defaultPoetryOverrides
    ];
    extraPackages = ps: [
      ps._poetry
      ps.tkinter
    ];
  };

  export.pythontools = std.buildEnv {
    name = "python tools";
    paths = [
      python-with-deps
      (std.buildEnv {
        name = "pyright";
        paths = [pyright];
        pathsToLink = ["/bin"];
      })
    ];
    postBuild = ''
      ln -s ${python-patched} $out/python-nodep
    '';
  };
}
