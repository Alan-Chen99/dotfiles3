{
  self,
  git,
  gnugrep,
  home-manager,
  home-manager-bin,
  legacypkgs,
  lib,
  fonts,
  nix-stable,
  nixpkgs-flakes,
  profile,
  std,
  system,
}: rec {
  # TODO: why?
  # export.home-manager-bin-wrapped =
  #   std.runCommandLocal "home-manager-bin" {
  #     nativeBuildInputs = [std.makeWrapper];
  #   } ''
  #     mkdir $out
  #     makeWrapper ${home-manager-bin}/bin/home-manager $out/bin/home-manager \
  #       --set PATH ${nix-stable}/bin:${git}/bin:${gnugrep}/bin
  #   '';

  # home-manager switch --flake .
  export.home = home-manager.lib.homeManagerConfiguration {
    # home-manager wants to pin its nixpkgs?
    pkgs = home-manager.inputs.nixpkgs.legacyPackages."${system}";

    # Specify your home configuration modules here, for example,
    # the path to your home.nix.
    modules = [
      (
        {
          config,
          pkgs,
          ...
        }: {
          # Home Manager needs a bit of information about you and the paths it should
          # manage.
          home.username = "alan";
          home.homeDirectory = "/home/alan";

          programs.git = {
            package = git;
            enable = true;
            userName = "Alan Chen";
            userEmail = "chenxinyang99@gmail.com";
          };

          fonts.fontconfig.enable = true;

          # This value determines the Home Manager release that your configuration is
          # compatible with. This helps avoid breakage when a new Home Manager release
          # introduces backwards incompatible changes.
          #
          # You should not change this value, even if you update Home Manager. If you do
          # want to update the value, then make sure to first check the Home Manager
          # release notes.
          home.stateVersion = "23.11"; # Please read the comment before changing.

          # The home.packages option allows you to install Nix packages into your
          # environment.
          home.packages = [
            fonts
          ];

          home.file = {
            # # Building this configuration will create a copy of 'dotfiles/screenrc' in
            # # the Nix store. Activating the configuration will then make '~/.screenrc' a
            # # symlink to the Nix store copy.
            # ".screenrc".source = dotfiles/screenrc;

            # # You can also set the file content immediately.
            # ".gradle/gradle.properties".text = ''
            #   org.gradle.console=verbose
            #   org.gradle.daemon.idletimeout=3600000
            # '';
          };

          # Home Manager can also manage your environment variables through
          # 'home.sessionVariables'. If you don't want to manage your shell through Home
          # Manager then you have to manually source 'hm-session-vars.sh' located at
          # either
          #
          #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
          #
          # or
          #
          #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
          #
          # or
          #
          #  /etc/profiles/per-user/alan/etc/profile.d/hm-session-vars.sh
          #
          home.sessionVariables = {
            # EDITOR = "emacs";
          };

          # Let Home Manager install and manage itself.
          # programs.home-manager.enable = true;
        }
      )
    ];

    # Optionally use extraSpecialArgs
    # to pass through arguments to home.nix
  };
}
