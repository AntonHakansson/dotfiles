{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.dwm;
  memory = pkgs.writeScriptBin "sb-memory" ''
    case $BLOCK_BUTTON in
      1) ${pkgs.libnotify}/bin/notify-send "Memory hogs" "$(${pkgs.procps}/bin/ps axch -o cmd:15,%mem --sort=-%mem | ${pkgs.coreutils}/bin/head)" ;;
      2) ${pkgs.util-linux}/bin/setsid -f "$TERMINAL" -e htop --sort PERCENT_MEM ;;
      3) ${pkgs.libnotify}/bin/notify-send "Memory module" "\- Shows Memory Used/Total.
          - Click to show memory hogs.
          - Middle click to open htop." ;;
      6) "$TERMINAL" -e "$EDITOR" "$0" ;;
    esac
    ${pkgs.procps}/bin/free --mebi | sed -n '2{p;q}' | ${pkgs.gawk}/bin/awk '{printf ("%2.1f/%2.1fGiB\n", ( $3 / 1024), ($2 / 1024))}'
  '';
  my-dwmblocks = (pkgs.dwmblocks.override {
    conf = ''
      static const Block blocks[] = {
        /*Icon*/  /*Command*/   /*Update Interval*/   /*Update Signal*/
        {"Mem: ", "${memory}/bin/sb-memory",  30,     14},
        {"",     "date +'%h %d (%a) %H:%M'",  60,     1},
      };

      static char *delim = " | ";
    '';
  });
in {
  options.modules.desktop.dwm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO
    # modules.theme.onReload.dwm = '''';

    environment.systemPackages = with pkgs; [ dunst libnotify ];
    user.packages = [ memory ];

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
      path = [ memory ];
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
