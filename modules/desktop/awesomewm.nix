{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.awesomewm;
in {
  options.modules.desktop.awesomewm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO
    # modules.theme.onReload.awesomewm = '''';

    environment.systemPackages = with pkgs; [ dunst libnotify ];

    services = {
      picom.enable = true;
      redshift.enable = true;
      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "none+awesome";
          lightdm.enable = true;
          lightdm.greeters.mini.enable = true;
        };
        desktopManager.xfce = {
          enable = true;
          thunarPlugins = with pkgs.xfce; [
            thunar-archive-plugin
            thunar_volman
          ];

          # Don't install XFCE desktop components (xfdesktop, panel and notification daemon)
          noDesktop = true;
        };
        windowManager.awesome.enable = true;
      };

      # Hide mouse cursor when inactive
      unclutter-xfixes.enable = true;
    };

    systemd.user.services.dunst = {
      enable = true;
      description = "Dunst notification daemon";
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        Restart = "always";
        RestartSec = 1;
        ExecStart = "${pkgs.dunst}/bin/dunst";
      };
    };
  };
}
