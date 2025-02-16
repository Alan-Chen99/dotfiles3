{
  self,
  legacypkgs,
  flakes,
}: rec {
  export.yaru-theme = legacypkgs.yaru-theme.overrideAttrs {
    src = flakes.yaru;
  };
}
