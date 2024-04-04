{
  self,
  gitignore-lib,
  flakes-self,
  lib,
}: rec {
  sourceInfo = flakes-self.sourceInfo;

  self-path = builtins.toString sourceInfo.outPath;

  export.cleansrc =
    if lib.strings.isStorePath self-path
    then src: src
    else
      src:
        lib.cleanSourceWith {
          filter = gitignore-lib.gitignoreFilter ../.;
          inherit src;
        };

  export.src = self.cleansrc self-path;
}
