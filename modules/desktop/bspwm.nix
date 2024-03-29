{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.bspwm;
  configDir = config.dotfiles.configDir;
in {
  options.modules.desktop.bspwm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    modules.theme.onReload.bspwm = ''
      ${pkgs.bspwm}/bin/bspc wm -r
      source $XDG_CONFIG_HOME/bspwm/bspwmrc
    '';

    environment.systemPackages = with pkgs; [
      lightdm
      dunst
      libnotify
      (polybar.override {
        pulseSupport = true;
        nlSupport = true;
      })
      unclutter
    ];

    services = {
      picom.enable = true;
      redshift.enable = true;
      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "none+bspwm";
          lightdm.enable = true;
          lightdm.greeters.mini.enable = true;
        };
        desktopManager.xfce = {
          enable = true;
          thunarPlugins = with pkgs.xfce; [
            thunar-archive-plugin
            thunar_volman
          ];

          # "Don't install XFCE desktop components (xfdesktop, panel and notification
          # daemon).";
          noDesktop = true;
        };
        windowManager.bspwm.enable = true;
      };
    };

    systemd.user.services."dunst" = {
      enable = true;
      description = "";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
    };

    systemd.user.services."unclutter" = {
      enable = true;
      description = "hide cursor after X seconds idle";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.unclutter}/bin/unclutter";
    };

    # link recursively so other modules can link files in their folders
    home.configFile = {
      "sxhkd".source = "${configDir}/sxhkd";
      "bspwm" = {
        source = "${configDir}/bspwm";
        recursive = true;
      };
    };
  };
}
