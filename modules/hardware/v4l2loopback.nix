{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.v4l2loopback;
in
{
  options.modules.hardware.v4l2loopback = {
    enable = mkEnableOption "Video4Linux2 device driver kernel module.";
    videoNr = mkOption {
      default = "20";
      type = types.str;
      description = ''
        video device number /dev/video{number}
      '';
    };
  };

  config = mkIf cfg.enable {
    boot.kernelModules = [ "videodev" "v4l2loopback" ];
    boot.extraModulePackages = with config.boot.kernelPackages;
      [ v4l2loopback ];
    boot.extraModprobeConfig = ''
      options v4l2loopback devices=1 exclusive_caps=1 video_nr=${cfg.videoNr} card_label="v4l2loopback"
    '';
    user.packages = with pkgs; [ unstable.droidcam ];
  };
}
