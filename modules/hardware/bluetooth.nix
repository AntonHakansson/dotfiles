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

      hardware.bluetooth.config = {
        General = { Enable = "Source,Sink,Media,Socket"; };
      };
    })
  ]);
}
