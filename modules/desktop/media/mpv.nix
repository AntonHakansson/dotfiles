{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.mpv;
  configDir = config.dotfiles.configDir;
in {
  options.modules.desktop.media.mpv = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (unstable.mpv-with-scripts.override {
        scripts = with unstable.mpvScripts; [ autoload ];
      })
      unstable.mpvc # CLI controller for mpv
      subdl
    ];

    home.configFile = {
      "mpv" = {
        source = "${configDir}/mpv";
        recursive = true;
      };
    };
  };
}
