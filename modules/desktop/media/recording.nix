{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.recording;
in {
  options.modules.desktop.media.recording = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt true;
    video.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
    # for recording and remastering audio
      (if cfg.audio.enable then [ audacity ] else [ ]) ++
      # for longer term streaming/recording the screen
      (if cfg.video.enable then [ obs-studio handbrake ] else [ ]);
  };
}
