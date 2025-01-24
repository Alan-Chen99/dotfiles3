{
  self,
  pypkgs-bins,
  std,
  super,
}: rec {
  export.ci-deps = std.buildEnv {
    name = "ci-deps";
    paths = [
      # alejandra
      # prettier
      pypkgs-bins.black
      pypkgs-bins.isort
    ];
  };
  export.ci-instantiate = std.buildEnv {
    name = "ci-instantiate";
    paths = [
      super.ci-deps
      # super.cxxtools
      super.emacs
      super.env-scripts
      super.fonts
      super.js
      # super.nixtools
      super.pdf-tools-epdfinfo
      super.pkgs-big
      super.pkgs-small
      super.profile
      super.profile-root
      super.python-all
      super.pythontools
      # super.schemat
    ];
  };
}
