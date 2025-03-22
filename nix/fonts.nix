{
  self,
  dejavu_fonts,
  nerdfonts,
  legacypkgs,
  flakes,
  pkgs-unstable,
  std,
  system,
}: rec {
  # pkgs = flakes.nixpkgs21-11.legacyPackages."${system}";

  export.fonts = std.buildEnv {
    name = "fonts";
    paths = [
      legacypkgs.fontconfig
      legacypkgs.fontconfig.out

      dejavu_fonts
      (nerdfonts.override {
        fonts = [
          "FiraCode"
          "DroidSansMono"
          "Iosevka"
          "Hack"
          "Inconsolata"
          "DejaVuSansMono"
        ];
      })
      legacypkgs.noto-fonts
      legacypkgs.noto-fonts-color-emoji
      # legacypkgs.noto-fonts-cjk
      legacypkgs.noto-fonts-cjk-sans
      # legacypkgs.noto-fonts-cjk-serif
      legacypkgs.noto-fonts-extra
    ];
  };
}
