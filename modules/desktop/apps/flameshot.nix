# Flameshot: simple to use screenshot software

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.flameshot;
in {
  options.modules.desktop.apps.flameshot = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ unstable.flameshot ];

    # https://github.com/flameshot-org/flameshot/blob/master/src/utils/confighandler.cpp#L76
    home.configFile."flameshot/flameshot.ini".text = ''
      [General]
      showHelp=false
      showSidePanelButton=false
      disabledTrayIcon=true
      showStartupLaunchMessage=false
      startupLaunch=false
      savePath=${config.user.home}/dl
      drawThickness=3
      uiColor=#FF79C6
      contrastUiColor=#51AFEF
    '';
  };
}
