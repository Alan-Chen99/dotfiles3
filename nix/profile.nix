{
  self,
  bashInteractive,
  coreutils,
  diffstat,
  diffutils,
  direnv,
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
  legacypkgs,
  lib,
  nix,
  nixmeta,
  nixrepl-wrapper,
  nixwrapper,
  patch,
  pdf-tools-epdfinfo,
  ripgrep,
  source-ver,
  std,
  taplo,
  tree,
  tree-sitter,
  tzdata,
  which,
}: rec {
  # TODO: add indirect deps, possibly from lock file, to prevent them from being garbage collected
  cmds-attrs = builtins.mapAttrs (name: val: "ln -s ${val} ${name}") (removeAttrs flakes ["self" "nixpkgs-lib"]);
  cmd-body = builtins.concatStringsSep "\n" (builtins.attrValues cmds-attrs);

  test = removeAttrs flakes ["self" "nixpkgs-lib"];

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

    XDG_DATA_DIRS = [
      "$HOME/.nix-profile/share"
      "$XDG_DATA_DIRS"
    ];
    SSL_CERT_FILE = "${legacypkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    # FONTCONFIG_FILE = "$HOME/.nix-profile/etc/fonts/fonts.conf";
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
      bashInteractive
      coreutils
      diffstat
      diffutils
      direnv
      fd
      file
      findutils
      gawk
      # gcc
      gdb
      git
      glibcLocales
      gnugrep
      gnused
      # home-manager-bin-wrapped
      keychain
      # TODO: doesnt build
      # nix.doc
      nixmeta
      nixrepl-wrapper
      nixwrapper
      patch
      # pdf-tools-epdfinfo
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
      mkdir -p $out/repos
      cd $out/repos
      ${cmd-body}
    '';
  };

  pkgs-small_ = {
    inherit
      (legacypkgs)
      cachix
      cmake
      cmake-format
      dockfmt
      ffmpeg
      git-lfs
      graphviz
      html-tidy
      hyperfine
      ispell
      kavita
      libtool
      nixfmt-classic
      nixpkgs-fmt
      shfmt
      stylua
      unzip
      zip
      ;
  };
  export.pkgs-small = std.buildEnv {
    name = "pkgs-small";
    paths = builtins.attrValues pkgs-small_;
  };

  pkgs-big_ = {
    inherit
      (legacypkgs)
      fontfinder
      jre
      racket
      ;
  };
  export.pkgs-big = std.buildEnv {
    name = "pkgs-big";
    paths = builtins.attrValues pkgs-big_;
  };
}
