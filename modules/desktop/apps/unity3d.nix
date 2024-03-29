# modules/dev/unity3d.nix --- https://unity.com

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.unity3d;
in {
  options.modules.desktop.apps.unity3d = { enable = mkBoolOpt false; };

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ unstable.unityhub ]; };
}
