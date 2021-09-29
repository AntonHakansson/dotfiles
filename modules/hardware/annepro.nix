{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.annepro;
in {
  options.modules.hardware.annepro = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {

    # https://www.reddit.com/r/AnnePro/comments/gruzcb/anne_pro_2_linux_cant_type_after_inactivity/
    boot.kernelParams = [ "usbhid.quirks=0x04D9:0xA292:0x00000400" ];

    user.packages = with pkgs;
      [
        # obinskit depend on electron which is marked insecure
        # unstable.obinskit
      ];
  };
}
