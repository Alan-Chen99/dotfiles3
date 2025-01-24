{
  self,
  legacypkgs,
  lib,
  nixpkgs-flakes,
  pkgs-unstable,
  poetry2nix,
  pyright,
  python,
  std,
  basedpyright,
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

  # ex: nix shell ..#python.inject.pip.ipython
  export.poetrypython = let
    injectwith = injected: (
      builtins.mapAttrs
      (name: val: (
        let
          new = injected ++ [name];
        in
          (poetrypython.withPackages (ps: map (x: ps."${x}") new)) // (injectwith new)
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

        regexfactory = prev.regexfactory.overridePythonAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or []) ++ [final.setuptools];
        });

        # this is bc defaultPoetryOverrides says poetry = poetry_core
        _poetry = prev.poetry;
      })
      poetry-lib.defaultPoetryOverrides
    ];
  };

  # for running scripts in this repo
  export.env-scripts = poetrypython.withPackages (ps: [
    ps.pygit2
    ps.regexfactory
  ]);

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
        # poetrypython
        # pyright
        basedpyright
      ];
    pathsToLink = ["/bin"];
    postBuild = ''
      mkdir -p $out/lib/python3.12/site-packages/
      ln -s ${python-patched} $out/python-nodep
    '';
  };

  pypkgs-bins_ = {
    inherit (poetrypython.pkgs) yt-dlp vcstool2;
    poetry = poetrypython.pkgs._poetry;
  };

  export.pypkgs-bins = builtins.mapAttrs (n: e:
    std.buildEnv {
      name = n;
      paths = [e];
      pathsToLink = ["/bin" "/etc" "/share"];
    })
  pypkgs-bins_;
}
