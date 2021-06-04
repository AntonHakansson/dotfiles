{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  hwCfg = config.modules.hardware;
  cfg = hwCfg.bluetooth;
in {
  options.modules.hardware.bluetooth = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      hardware.bluetooth.enable = true;
      hardware.bluetooth.package = pkgs.unstable.bluezFull;
    }

    (mkIf cfg.audio.enable {
      # services.blueman.enable = true;
      environment.systemPackages = [ pkgs.unstable.blueman ];
      services.dbus.packages = [ pkgs.unstable.blueman ];
      systemd.packages = [ pkgs.unstable.blueman ];

      user.packages = with pkgs; [ unstable.bluez-tools  ];

      hardware.pulseaudio = {
        # NixOS allows either a lightweight build (default) or full build of
        # PulseAudio to be installed.  Only the full build has Bluetooth
        # support, so it must be selected here.
        package = pkgs.unstable.pulseaudioFull;
        # Enable additional codecs
        extraModules = [ pkgs.unstable.pulseaudio-modules-bt ];
      };
      hardware.bluetooth.config = {
        General = { Enable = "Source,Sink,Media,Socket"; };
      };
    })
  ]);
}
