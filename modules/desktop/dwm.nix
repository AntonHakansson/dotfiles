{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.dwm;
  # configDir = config.dotfiles.configDir;
  my-dwmblocks = (pkgs.dwmblocks.override {
    conf = ''
      static const Block blocks[] = {
        /*Icon*/  /*Command*/   /*Update Interval*/   /*Update Signal*/
        {"Mem: ", "free -h | awk '/^Mem/ { print $3\"/\"$2 }' | sed s/i//g",    30,     0},
        {"",     "date +'%h %d (%a) %H:%M'",                                   60,     0},
      };

      static char delim[] = " | ";
      static unsigned int delimLen = 5;
    '';
  });
in {
  options.modules.desktop.dwm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO
    # modules.theme.onReload.dwm = '''';

    environment.systemPackages = with pkgs; [
      lightdm
      dunst
      libnotify
      (polybar.override {
        pulseSupport = true;
        nlSupport = true;
      })
      my-dwmblocks
    ];

    services = {
      picom.enable = true;
      redshift.enable = true;
      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "none+dwm";
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
        windowManager.dwm.enable = true;
      };
      unclutter-xfixes = { enable = true; };
    };

    systemd.user.services."dwmblocks" = {
      enable = true;
      description = "Modular status bar for dwm written in c";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${my-dwmblocks}/bin/dwmblocks";
      path = [ pkgs.procps pkgs.gawk ];
    };

    systemd.user.services."dunst" = {
      enable = true;
      description = "Lightweight and customizable notification daemon";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
    };
  };
}
