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
        date -d @${builtins.toString date} +%Y-%m-%d--%H:%M:%S >> $out
      '';
  in
    lib.strings.removeSuffix "\n" (builtins.readFile drv);

  lastmodified =
    if sourceInfo ? lastModified
    then format-date sourceInfo.lastModified
    else null;
  hash =
    if sourceInfo ? shortRev
    then sourceInfo.shortRev
    else if sourceInfo ? dirtyShortRev
    then sourceInfo.dirtyShortRev
    # older nix versions copy flake source to store
    else if sourceInfo ? narHash
    then builtins.substring 7 7 sourceInfo.narHash
    else null;

  export.version =
    if version-file != null
    then version-file
    # else if sourceInfo ? rev && sourceInfo ? lastModified
    # then lastmodified
    else if sourceInfo ? lastModified && hash != null
    then "${lastmodified}-${hash}"
    else version-none;

  nixpkgs-ver = lib.strings.removeSuffix "\n" (builtins.readFile "${nixpkgs-flakes}/.version");

  metadata = lib.concatStringsSep "\n" [
    ''version: ${self.version}''
    ''nix: ${nix}''
    ''nixpkgs: ${nixpkgs-ver}; ${format-date nixpkgs-flakes.sourceInfo.lastModified}''
    ''nixpkgs: ${lib.generators.toPretty {} nixpkgs-flakes.sourceInfo}''
    # behaves differently when evaluated from p# vs .#
    # ''info: ${lib.generators.toPretty {} (builtins.removeAttrs sourceInfo ["outPath"])}''
    ""
  ];

  export.source-ver = std.runCommandLocal "source-ver" {} ''
    mkdir $out
    cp -r "${src}"/. $out
    echo "${self.version}" >> $out/version
    echo ${lib.strings.escapeShellArg metadata} >> $out/metadata
  '';

  export.nixmeta =
    std.writeScriptBin "nixmeta"
    ''
      #! ${std.stdenv.shell}
      ${coreutils}/bin/echo "source: ${self.source-ver}"
      ${coreutils}/bin/cat ${self.source-ver}/metadata
    '';
}
