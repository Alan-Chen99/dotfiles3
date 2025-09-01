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
  # TODO: (25-5) i think this was here bc i was trying to install it on a bare docker (prob distroless?)
  # export.home-manager-bin-wrapped =
  #   std.runCommandLocal "home-manager-bin" {
  #     nativeBuildInputs = [std.makeWrapper];
  #   } ''
  #     mkdir $out
  #     makeWrapper ${home-manager-bin}/bin/home-manager $out/bin/home-manager \
  #       --set PATH ${nix-stable}/bin:${git}/bin:${gnugrep}/bin
  #   '';

  export.user-name = "alan";
  user = self.user-name;

  # home-manager switch --flake .
  # some updates invalidates cache
  # TODO: why dont home-manger run this on switch?
  # fc-cache -fv
  export.home = home-manager.lib.homeManagerConfiguration {
    # home-manager wants to pin its nixpkgs?
    pkgs = home-manager.inputs.nixpkgs.legacyPackages."${system}";

    # Specify your home configuration modules here, for example,
    # the path to your home.nix.
    modules = [
      (
        {
          config,
          _pkgs,
          ...
        }: {
          # Home Manager needs a bit of information about you and the paths it should
          # manage.
          home.username = "${user}";
          home.homeDirectory = "/home/${user}";

          # programs.git = {
          #   package = git;
          #   enable = true;
          #   userName = "Alan Chen";
          #   userEmail = "chenxinyang99@gmail.com";
          # };

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

          gtk = {
            enable = true;

            # iconTheme = {
            #   name = "Adwaita";
            #   # package = pkgs.papirus-icon-theme;
            # };

            # font = {
            #   name = "Hack Nerd Font";
            #   size = 14;
            # };

            # theme = {
            #   name = "palenight";
            #   package = pkgs.palenight-theme;
            # };

            # cursorTheme = {
            #   # name = "Hackneyed";
            #   name = "Adwaita";
            #   # package = legacypkgs.numix-cursor-theme;
            # };
          };

          # dconf dump /
          # gsettings list-recursively
          dconf.settings = {
            # "org/gnome/shell" = {
            # };
            "org/gnome/desktop/interface" = let
              theme = "Yaru";
            in {
              gtk-theme = theme;
              icon-theme = theme;
              cursor-theme = theme;
              # cursor-theme = "Adwaita";
              # cursor-theme = "Hackneyed";

              document-font-name = "Cantarell 14";
              font-name = "Cantarell 14";
              monospace-font-name = "Hack Nerd Font 20";
            };
            # TODO: dont work
            "org/gnome/desktop/wm/preferences" = {
              titlebar-font = "Cantarell Bold 10";
              titlebar-uses-system-font = false;
            };
          };
        }
      )
    ];

    # Optionally use extraSpecialArgs
    # to pass through arguments to home.nix
  };
}
