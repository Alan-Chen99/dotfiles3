{
  self,
  lib,
  nixpkgs-flakes,
  legacypkgs,
  poetry2nix,
  pyright,
  python,
  std,
}: rec {
  # pkgs = legacypkgs;
  # outputs.legacyPackages.x86_64-linux._self.default.__mods.python.__out

  poetry-lib = poetry2nix.lib.mkPoetry2Nix {pkgs = legacypkgs;};

  poetrypython = self.poetrypython.python;

  python-patched = python.override {
    self = python-patched;
    packageOverrides = final: prev: {
      pip = poetrypython.pkgs.pip;
    };
  };

  poetryPackages-attrs = builtins.listToAttrs (
    map (drv: {
      name = drv.pname;
      value = drv;
    })
    self.poetrypython.poetryPackages
  );

  export.poetrypython = let
    injectwith = injected: (
      builtins.mapAttrs
      (name: val: (
        let
          new = injected ++ [name];
        in
          (
            poetrypython.withPackages (ps: map (x: ps."${x}") new)
          )
          // {inject = injectwith new;}
      ))
      poetryPackages-attrs
    );
  in
    _poetrypython
    // {
      python =
        _poetrypython.python
        // {
          inject = injectwith [];
        };
    };

  _poetrypython = poetry-lib.mkPoetryPackages {
    python = python-patched;
    projectDir = ./.;
    preferWheels = true;
    overrides = [
      (final: prev: rec {
        # typing is builtin since python 3.6, ignore a dependency on typing module in pypi
        typing = null;

        cchardet = prev.cchardet.override {preferWheel = false;};

        # final.pkgs.meson (meson with poetrypython) fails check so disable checks for now
        # TODO: override dont work here bc poetry2nix use overrideAttrs to disable checks first
        # and apparently you cant override after a overrideAttrs
        pythonix = final.callPackage (nixpkgs-flakes + /pkgs/development/python-modules/pythonix) {
          nix = prev.pkgs.nixVersions.nix_2_3;
          meson =
            (prev.pkgs.meson.override {
              python3 = final.python;
            })
            .overrideAttrs {
              doCheck = false;
              doInstallCheck = false;
            };
        };

        nixpkgs = prev.nixpkgs.overridePythonAttrs (old: {
          propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ [final.pythonix final.ninja];
        });

        # this is bc defaultPoetryOverrides says poetry = poetry_core
        _poetry = prev.poetry;
      })
      poetry-lib.defaultPoetryOverrides
    ];
  };

  export.python-all = poetrypython.withPackages (ps:
    self.poetrypython.poetryPackages
    ++ [
      ps._poetry
      ps.tkinter
    ]);

  export.pythonlibs = std.buildEnv {
    name = "python libs";
    paths = [
      self.python-all
    ];
    pathsToLink = [
      "/lib"
    ];
  };

  defaultdeps = {
    inherit
      (poetrypython.pkgs)
      _poetry
      pip
      ipython
      black
      isort
      pybind11
      ;
  };

  export.pythontools = std.buildEnv {
    name = "python tools";
    paths =
      (builtins.attrValues defaultdeps)
      ++ [
        poetrypython
        pyright
      ];
    pathsToLink = ["/bin"];
    postBuild = ''
      mkdir -p $out/lib/python3.12/site-packages/
      ln -s ${python-patched} $out/python-nodep
    '';
  };

  export.youtube-dl = std.buildEnv {
    name = "youtube-dl";
    paths = [poetrypython.pkgs.youtube-dl];
    pathsToLink = ["/bin" "/etc" "/share"];
  };
}
