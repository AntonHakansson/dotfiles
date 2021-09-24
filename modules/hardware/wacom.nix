{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.wacom;
in
{
  options.modules.hardware.wacom = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # For my intuos4 pro. Doesn't work for cintiqs.
    services.xserver.wacom.enable = true;
    # TODO Move this to udev
    user.packages = with pkgs;
      [
        (writeScriptBin "wacom" ''
          #!${stdenv.shell}
          # lock tablet to main display
          if xinput list --id-only "Wacom Intuos BT M Pen stylus" 2>&1 >/dev/null; then
            xinput map-to-output $(xinput list --id-only "Wacom Intuos BT M Pen stylus") DP-1
          fi
        '')
      ];
  };
}
