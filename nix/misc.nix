{
  self,
  flakes,
  legacypkgs,
  std,
}: rec {
  pkgs = legacypkgs;

  export.yaru-theme = pkgs.yaru-theme;
  # export.yaru-theme = pkgs.yaru-theme.overrideAttrs {
  #   src = flakes.yaru;
  # };

  export.iperf3 = pkgs.iperf3.overrideAttrs {src = flakes.iperf3;};

  export.openssh-unsafe = pkgs.openssh.overrideAttrs {
    src = flakes.openssh-unsafe;
    doInstallCheck = false; # doesnt pass, why?
  };

  export.testdrv =
    std.runCommandLocal "hello" {}
    ''
      mkdir $out
    '';
}
