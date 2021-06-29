{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gaming.steam;
  my_steam = (pkgs.unstable.steam.override {
    extraLibraries = pkgs: [ pkgs.pipewire ];
  });
in {
  options.modules.desktop.gaming.steam = with types; {
    enable = mkBoolOpt false;
    hardware.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.steam.enable = true;
      programs.gamemode.enable = true;

      user.packages = with pkgs.unstable; [ vulkan-tools vulkan-headers mangohud lutris ];

      # better for steam proton games
      systemd.extraConfig = "DefaultLimitNOFILE=1048576";
    }

    (mkIf cfg.hardware.enable { hardware.steam-hardware.enable = true; })
  ]);
}
