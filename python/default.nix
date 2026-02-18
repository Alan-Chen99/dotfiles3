{
  self,
  std,
  legacypkgs,
  lib,
  flakes,
  python-nopkgs,
  nix-filter,
}: rec {
  workspace = flakes.uv2nix.lib.workspace.loadWorkspace {
    workspaceRoot = nix-filter {
      root = ./.;
      include = [
        ./pyproject.toml
        ./uv.lock
      ];
    };
  };

  overlay = workspace.mkPyprojectOverlay {
    sourcePreference = "wheel"; # or sourcePreference = "sdist";
  };

  buildSystemOverrides = final: prev:
    builtins.mapAttrs (
      name: spec:
        prev.${name}.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ final.resolveBuildSystem spec;
        })
    )
    (import ./buildSystemOverrides.nix);

  pyprojectOverrides = final: prev: {
    python-magic = let
      magic-nixpkgs = legacypkgs.python313Packages.python-magic;
    in
      (prev.python-magic.override {sourcePreference = "sdist";}).overrideAttrs (old:
        assert old.version == magic-nixpkgs.version; {
          src = magic-nixpkgs.src;
          patches = magic-nixpkgs.patches;
        });
  };

  # Construct package set
  pythonSet =
    # Use base package set from pyproject.nix builders
    (legacypkgs.callPackage flakes.pyproject-nix.build.packages {
      python = python-nopkgs;
    })
    .overrideScope
    (
      lib.composeManyExtensions [
        overlay
        buildSystemOverrides
        (flakes.uv2nix_hammer_overrides.overrides legacypkgs)
        pyprojectOverrides
        # pkgs.callPackage flakes.pyproject-nix.build.hacks {}

        (
          # from uv2nix/templates/hello-tkinter
          final: _prev: let
            inherit (final) pkgs;
            hacks = pkgs.callPackage flakes.pyproject-nix.build.hacks {};
          in {
            tkinter = hacks.nixpkgsPrebuilt {
              from = python-nopkgs.pkgs.tkinter;
            };
          }
        )
      ]
    );

  ####################

  export.uv-workspace = workspace;
  export.pypkgs = pythonSet;
  export.pypkgs-all = pythonSet.mkVirtualEnv "venv-all-uv-packages" (workspace.deps.all // {tkinter = [];});

  export.pythonlibs = std.buildEnv {
    name = "python libs";
    paths = [
      self.pypkgs-all
    ];
    pathsToLink = [
      "/lib"
    ];
  };

  ####################

  _python-inject = let
    injectwith = injected: let
      drv = pythonSet.mkVirtualEnv "inject-${python-nopkgs.name}" injected;
    in (
      (
        builtins.mapAttrs
        (name: _val: injectwith (injected // {"${name}" = [];}))
        pythonSet
      )
      // {
        inherit (drv) type name drvPath meta;
        _drv = drv;
      }
    );
  in
    injectwith {};

  export.python-inject = python-nopkgs // {inject = _python-inject;};

  ####################

  mkApplication = (legacypkgs.callPackages flakes.pyproject-nix.build.util {}).mkApplication;

  make_app = name:
    mkApplication {
      venv = pythonSet.mkVirtualEnv "${name}-env" {"${name}" = [];};
      package = pythonSet."${name}";
    };

  pythontools_ = {
    inherit
      (pythonSet)
      autoflake
      basedpyright
      black
      ipython
      isort
      pyright
      ;
  };
  pypkgs-bins_ =
    pythontools_
    // {
      inherit
        (pythonSet)
        diceware
        yt-dlp
        ;
    };

  export.pypkgs-bins =
    builtins.mapAttrs (n: _: (make_app n)) pypkgs-bins_;

  export.pythontools = std.buildEnv {
    name = "python tools";
    paths =
      builtins.attrValues
      (builtins.mapAttrs (n: _: (make_app n)) pythontools_);
  };
}
