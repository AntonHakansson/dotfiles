# zig

{ config, lib, pkgs, inputs, utils, ... }:

with builtins;
with lib;
with lib.my;
let cfg = config.modules.dev.zig;
in {
  options.modules.dev.zig = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages =
      [ inputs.zig-overlay.packages."x86_64-linux".master.latest pkgs.zls ];
  };
}
