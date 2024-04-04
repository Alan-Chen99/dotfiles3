{
  self,
  coreutils,
  flakes-self,
  lib,
  nix,
  nixpkgs-flakes,
  src,
  std,
  tzdata,
  which,
}: rec {
  sourceInfo = flakes-self.sourceInfo;

  version-file =
    if builtins.pathExists ../version
    then lib.strings.removeSuffix "\n" (builtins.readFile ../version)
    else null;

  version-none = "0000000";

  format-date = date: let
    drv =
      std.runCommandLocal "format-date" {
        nativeBuildInputs = [tzdata];
      } ''
        export TZ='America/New_York'
        date -d @${builtins.toString date} +%Y-%m-%d-%H-%M-%S >> $out
      '';
  in
    lib.strings.removeSuffix "\n" (builtins.readFile drv);

  lastmodified =
    if sourceInfo ? lastModified
    then format-date sourceInfo.lastModified
    else null;

  export.version =
    if version-file != null
    then version-file
    else if sourceInfo ? rev && sourceInfo ? lastModified
    then lastmodified
    else if sourceInfo ? narHash
    then let
      hash = builtins.substring 7 7 sourceInfo.narHash;
    in
      if sourceInfo ? lastModified
      then "${lastmodified}-${hash}"
      else hash
    else version-none;

  nixpkgs-ver = lib.strings.removeSuffix "\n" (builtins.readFile "${nixpkgs-flakes}/.version");

  metadata = lib.concatStringsSep "\n" [
    ''version: ${self.version}''
    ''nix: ${nix}''
    ''nixpkgs: ${nixpkgs-ver}; ${format-date nixpkgs-flakes.sourceInfo.lastModified}''
    ''nixpkgs: ${lib.generators.toPretty {} nixpkgs-flakes.sourceInfo}''
    ''info: ${lib.generators.toPretty {} (builtins.removeAttrs sourceInfo ["outPath"])}''
    ""
  ];

  added-version-file = std.runCommandLocal "source" {} ''
    mkdir $out
    cp -r "${src}"/. $out
    echo "${self.version}" >> $out/version
    echo ${lib.strings.escapeShellArg metadata} >> $out/metadata
  '';

  export.source-ver =
    if version-file != null
    then "${src}"
    else if self.version != version-none
    then "${added-version-file}"
    else "${src}";

  export.nixmeta =
    std.writeScriptBin "nixmeta"
    ''
      #! ${std.stdenv.shell}
      ${coreutils}/bin/echo "source: ${self.source-ver}"
      ${coreutils}/bin/cat ${self.source-ver}/metadata
    '';
}
