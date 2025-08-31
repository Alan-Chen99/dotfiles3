{
  self,
  legacypkgs,
  flakes,
  pkgs-unstable,
  std,
  system,
}: rec {
  # pkgs = flakes.nixpkgs21-11.legacyPackages."${system}";

  fonts_ = {
    inherit
      (legacypkgs)
      dejavu_fonts
      #
      noto-fonts
      noto-fonts-color-emoji
      # noto-fonts-cjk
      noto-fonts-cjk-sans
      # noto-fonts-cjk-serif
      noto-fonts-extra
      ;

    inherit
      (legacypkgs.nerd-fonts)
      fira-code
      droid-sans-mono
      iosevka
      hack
      inconsolata
      dejavu-sans-mono
      ;
  };

  export.fonts = std.buildEnv {
    name = "fonts";
    paths =
      [
        legacypkgs.fontconfig
        legacypkgs.fontconfig.out
      ]
      ++ builtins.attrValues fonts_;
  };
}
