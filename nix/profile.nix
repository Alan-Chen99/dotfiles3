{
  self,
  bashInteractive,
  coreutils,
  diffstat,
  diffutils,
  direnv,
  emacs,
  fd,
  file,
  findutils,
  flakes,
  gawk,
  gcc,
  gdb,
  git,
  glibcLocales,
  gnugrep,
  gnused,
  home-manager-bin-wrapped,
  keychain,
  lib,
  nix,
  nixmeta,
  nixrepl-wrapper,
  nixwrapper,
  patch,
  ripgrep,
  source-ver,
  std,
  taplo,
  tree,
  tree-sitter,
  tzdata,
  which,
}: rec {
  repos-links = let
    cmds-attrs = builtins.mapAttrs (name: val: "ln -s ${val} ${name}") (removeAttrs flakes ["self" "nixpkgs-lib"]);
    cmd-body = builtins.concatStringsSep "\n" (builtins.attrValues cmds-attrs);
  in
    std.runCommandLocal "repos" {} ''
      mkdir $out
      cd $out
      mkdir repos
      cd repos
      ${cmd-body}
    '';

  profile-env = {
    # PATH = [
    #   "$HOME/.nix-profile/bin"
    #   "$PATH"
    # ];
    MANPATH = [
      "$HOME/.nix-profile/share/man"
      "/usr/share/man"
    ];
    INFOPATH = [
      "$HOME/.nix-profile/share/info"
      "/usr/share/info"
    ];
    LOCALE_ARCHIVE = "$HOME/.nix-profile/lib/locale/locale-archive";
    TZDIR = "$HOME/.nix-profile/share/zoneinfo/";
  };

  profile-text = let
    env-str =
      builtins.mapAttrs (
        _: val:
          if builtins.isString val
          then val
          else builtins.concatStringsSep ":" val
      )
      profile-env;
  in
    builtins.concatStringsSep "\n" (builtins.attrValues (
      builtins.mapAttrs (
        name: val: "export ${name}=${val}"
      )
      env-str
    ));

  export.profile = std.buildEnv {
    name = "profile";
    checkCollisionContents = false;
    paths = [
      # nix.man
      repos-links

      bashInteractive
      coreutils
      diffstat
      diffutils
      direnv
      emacs
      fd
      file
      findutils
      gawk
      gcc
      gdb
      git
      glibcLocales
      gnugrep
      gnused
      home-manager-bin-wrapped
      keychain
      nix.doc
      nixmeta
      nixrepl-wrapper
      nixwrapper
      patch
      ripgrep
      taplo
      tree
      tree-sitter
      tzdata.out
      which
    ];
    extraOutputsToInstall = ["man" "doc" "info"];
    pathsToLink = [
      "/bin"
      "/etc"
      "/lib/locale"
      "/share"
      "/repos"
    ];
    postBuild = ''
      mkdir $out/env
      echo ${lib.strings.escapeShellArg profile-text} >> $out/env/.env
      ln -s ${source-ver} $out/src
      ln -s $out $out/profile
    '';
  };
}
