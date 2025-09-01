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
      #
      super.pkgs-big
      super.pkgs-small
      #
      super.profile
      super.profile-root
      #
      super.emacs
      super.fonts
      super.home.activationPackage
      super.pdf-tools-epdfinfo
      #
      super.experimental.packages
      super.jspkgs-bins
      super.pypkgs-bins
      super.rustpkgs-bins
      #
      super.cxxtools
      super.js
      super.latexenv
      super.nixtools
      super.pypkgs-all
      super.pythontools
    ];
  };
}
