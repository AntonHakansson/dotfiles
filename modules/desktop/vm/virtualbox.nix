{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.vm.virtualbox;
in {
  options.modules.desktop.vm.virtualbox = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    virtualisation.virtualbox.host = {
      enable = true;
      # urg, takes so long to build, but needed for macOS guest
      # enableExtensionPack = true;
    };

    virtualisation.libvirtd.enable = true;

    programs.dconf.enable = true;
    environment.systemPackages = with pkgs; [ virt-manager ];

    user.extraGroups = [ "vboxusers" "libvirtd" ];
  };
}
