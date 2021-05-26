{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.annepro;
in {
  options.modules.hardware.annepro = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # obinskit depend on electron which is marked insecure
      # unstable.obinskit
    ];
  };
}