{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.nvidia;
in
{
  options.modules.hardware.nvidia = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    hardware.opengl = {
      enable = true;
      # https://nixos.wiki/wiki/Accelerated_Video_Playback
      extraPackages = with pkgs; [
        intel-media-driver
        vaapiVdpau
        libvdpau-va-gl
      ];
    };

    services.xserver.videoDrivers = [ "nvidia" ];
    services.xserver.deviceSection = ''
      Option      "Coolbits" "31"
    '';

    environment.systemPackages = with pkgs;
      [
        # Respect XDG conventions, damn it!
        (writeScriptBin "nvidia-settings" ''
          #!${stdenv.shell}
          mkdir -p "$XDG_CONFIG_HOME/nvidia"
          exec ${config.boot.kernelPackages.nvidia_x11.settings}/bin/nvidia-settings --config="$XDG_CONFIG_HOME/nvidia/settings"
        '')
      ];
  };
}
