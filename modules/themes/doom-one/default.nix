{ options, config, lib, pkgs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;
in
{
  config = mkIf (cfg.active == "doom-one") (mkMerge [
    # Desktop-agnostic configuration
    {
      modules = {
        theme = {
          wallpaper = mkDefault ./config/wallpaper.png;
          gtk = {
            theme = "Arc-Dark";
            iconTheme = "Arc";
            cursorTheme = "Paper";
          };
        };

        shell.zsh.rcFiles = [ ./config/zsh/prompt.zsh ];
        # shell.tmux.rcFiles = [ ./config/tmux.conf ];
        desktop.browsers = {
          firefox.userChrome = concatMapStringsSep "\n" readFile
            [ ./config/firefox/userChrome.css ];
        };
      };
    }

    # Desktop (X11) theming
    (mkIf config.services.xserver.enable {
      user.packages = with pkgs; [
        arc-theme
        arc-icon-theme
        paper-icon-theme # for rofi
      ];
      fonts = {
        fonts = with pkgs; [
          fira-code
          fira-code-symbols
          jetbrains-mono
          siji
          font-awesome-ttf
        ];
        fontconfig.defaultFonts = {
          sansSerif = [ "Fira Sans" ];
          monospace = [ "Fira Code" ];
        };
      };

      services.xserver.desktopManager.wallpaper.mode = "fill";

      # Compositor
      home-manager.users.${config.user.name}.services.picom = {
        # Round corner support -- https://github.com/ibhagwan/picom
        package = pkgs.nur.repos.reedrw.picom-next-ibhagwan;
        fade = true;
        fadeDelta = 1;
        shadow = true;
        shadowOffsets = [ (-5) (-5) ];
        extraOptions = ''
          shadow-radius = 12;
          shadow-exclude = [
            "name = 'Notification'",
            "class_g = 'Conky'",
            "class_g = 'Notify-osd'",
            "class_g = 'Cairo-clock'",
            "class_g = 'slop'",
            "class_g = 'Polybar'",
            "class_g = 'awesome'",
            "_GTK_FRAME_EXTENTS@:c"
          ];

          detect-rounded-corners = true;
          corner-radius = 8;
          rounded-corners-exclude = [
            "class_g = 'awesome'",
            "class_g = 'menu'",
            "class_g = 'i3status'",
            "class_g = 'nitrogen'",
            "class_g = 'Polybar'",
            "window_type = 'tooltip'",
            "window_type = 'desktop'"
          ];

          round-borders = 1;
        '';
      };

      # Login screen theme
      services.xserver.displayManager.lightdm.greeters.mini.extraConfig = ''
        text-color = "#ff79c6"
        password-background-color = "#1E2029"
        window-color = "#181a23"
        border-color = "#181a23"
      '';

      # Other dotfiles
      home.configFile = with config.modules;
        mkMerge [
          {
            # Sourced from sessionCommands in modules/themes/default.nix
            "xtheme/90-theme".source = ./config/Xresources;
          }
          (mkIf desktop.bspwm.enable {
            "bspwm/rc.d/polybar".source = ./config/polybar/run.sh;
            "bspwm/rc.d/theme".source = ./config/bspwmrc;
          })
          (mkIf desktop.apps.rofi.enable {
            "rofi/theme" = {
              source = ./config/rofi;
              recursive = true;
            };
          })
          (mkIf desktop.bspwm.enable {
            "polybar" = {
              source = ./config/polybar;
              recursive = true;
            };
          })
          (mkIf (desktop.bspwm.enable || desktop.dwm.enable || desktop.awesomewm.enable) {
            "dunst/dunstrc".source = ./config/dunstrc;
          })
          (mkIf desktop.term.alacritty.enable {
            "alacritty/alacritty.yml".source = ./config/alacritty.yml;
          })
        ];
    })
  ]);
}
