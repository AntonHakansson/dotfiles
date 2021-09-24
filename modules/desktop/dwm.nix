{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.dwm;
  sb-audio = pkgs.writeScriptBin "sb-audio" ''
    case $BUTTON in
      1) ${pkgs.pavucontrol}/bin/pavucontrol --tab=3
        ;;
    esac
    ${pkgs.alsa-utils}/bin/amixer -M sget Master | ${pkgs.gnugrep}/bin/grep -o -E [[:digit:]]+% | ${pkgs.coreutils}/bin/head -n 1
  '';
  sb-memory = pkgs.writeScriptBin "sb-memory" ''
    case $BUTTON in
      1) ${pkgs.libnotify}/bin/notify-send "Memory hogs" "$(${pkgs.procps}/bin/ps axch -o cmd:15,%mem --sort=-%mem | ${pkgs.coreutils}/bin/head)"
        ;;
      2) ${pkgs.alacritty}/bin/alacritty -e ${pkgs.htop}/bin/htop --sort PERCENT_MEM
        ;;
      3) ${pkgs.libnotify}/bin/notify-send "Memory module" "\- Shows Memory Used/Total.
          - Click to show memory hogs.
          - Middle click to open htop."
        ;;
    esac
    ${pkgs.procps}/bin/free | sed -n '2{p;q}' | ${pkgs.gawk}/bin/awk '{printf ("%.0f%", $3/$2 * 100.0)}'
  '';
  sb-fs = pkgs.writeScriptBin "sb-fs" ''
    ${pkgs.coreutils}/bin/df ~/ | ${pkgs.coreutils}/bin/tail -n 1 | ${pkgs.gawk}/bin/awk -F ' +' '{print $5}'
  '';
  sb-date = pkgs.writeScriptBin "sb-date" ''
    case $BUTTON in
      1) ${pkgs.firefox}/bin/firefox --new-window "calendar.google.com"
        ;;
      2) ${pkgs.alacritty}/bin/alacritty -e ${pkgs.htop}/bin/htop
        ;;
      3) ${pkgs.coreutils}/bin/date -u +"%Y-%m-%d" | ${pkgs.coreutils}/bin/tr -d "\n" | ${pkgs.xclip}/bin/xclip -sel clip && ${pkgs.libnotify}/bin/notify-send "Copied date to clipboard"
        ;;
    esac
    ${pkgs.coreutils}/bin/date +'%h %d (%a) %H:%M'
  '';
  dwmblocks = (pkgs.dwmblocks.override {
    conf = ''
      static const Block blocks[] = {
        /*Icon*/  /*Command*/     /*Update Interval*/   /*Update Signal*/
        {"RAM: ", "${sb-memory}/bin/sb-memory",  30,     14},
        {"/: ",   "${sb-fs}/bin/sb-fs",          60,     15},
        {"",      "${sb-audio}/bin/sb-audio",    30,     16},
        {"",      "${sb-date}/bin/sb-date",      60,      1},
      };

      static char delim[] = " | ";
      static unsigned int delimLen = 5;
    '';
  });
in
{
  options.modules.desktop.dwm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO
    # modules.theme.onReload.dwm = '''';

    environment.systemPackages = with pkgs; [ dunst libnotify ];

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

      # Hide mouse cursor when inactive
      unclutter-xfixes.enable = true;
    };

    systemd.user.services.dwmblocks = {
      enable = true;
      description = "Modular status bar for dwm written in c";
      wantedBy = [ "default.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.ExecStart = "${dwmblocks}/bin/dwmblocks";
    };

    systemd.user.services.dunst = {
      enable = true;
      description = "Dunst notification daemon";
      wantedBy = [ "default.target" ];
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
