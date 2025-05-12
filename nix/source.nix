{
  self,
  flakes-self,
  gitignore-lib,
  lib,
  nix-filter,
}: rec {
  sourceInfo = flakes-self.sourceInfo;

  self-path = builtins.toString sourceInfo.outPath;

  export.cleansrc =
    if lib.strings.isStorePath self-path
    then
      src:
        nix-filter {
          root = src;
          exclude = [
            "version"
            "metadata"
          ];
        }
    else
      src:
        lib.cleanSourceWith {
          filter = gitignore-lib.gitignoreFilter ../.;
          inherit src;
        };

  export.src = self.cleansrc ../.;
}
