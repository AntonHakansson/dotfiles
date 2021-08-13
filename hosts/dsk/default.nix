{ pkgs, config, lib, ... }:
let secrets = import ./secrets/secrets.nix;
in {
  imports = [ ../home.nix ./hardware-configuration.nix ];

  ## Modules
  modules = {
    desktop = {
      dwm.enable = true;
      apps = {
        discord.enable = true;
        rofi.enable = true;
        unity3d.enable = true;
      };
      browsers = {
        default = "firefox";
        brave.enable = true;
        firefox.enable = true;
      };
      gaming = { steam.enable = true; };
      media = {
        # daw.enable = true;
        documents.enable = true;
        graphics.enable = true;
        mpv.enable = true;
        recording.enable = true;
        spotify.enable = true;
      };
      term = {
        default = "alacritty";
        alacritty.enable = true;
      };
      vm = { virtualbox.enable = true; };
    };
    dev = {
      zig.enable = true;
      cc.enable = true;
      python.enable = true;
      shell.enable = true;
    };
    editors = {
      default = "nvim";
      emacs.enable = true;
      vim.enable = true;
      vscode.enable = true;
    };
    shell = {
      bitwarden.enable = true;
      direnv.enable = true;
      git.enable = true;
      gnupg.enable = true;
      # tmux.enable   = true;
      zsh.enable = true;
    };
    services = {
      ssh.enable = true;
      transmission.enable = true;
    };
    hardware = {
      audio.enable = true;
      nvidia.enable = true;
      annepro.enable = true;
      bluetooth.enable = true;
      wacom.enable = true;
      sensors.enable = true;
      v4l2loopback.enable = true;
    };
    crypto.enable = true;
    theme.active = "doom-one";
  };

  ## Local config
  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;
  # networking.networkmanager.enable = true;

  # CPU
  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = "performance";

  # Monitors
  services.xserver = {
    # This must be done manually to ensure my screen spaces are arranged exactly
    # as I need them to be *and* the correct monitor is "primary".
    monitorSection = ''
      VendorName     "Unknown"
      ModelName      "Acer GN246HL"
      HorizSync       30.0 - 160.0
      VertRefresh     56.0 - 144.0
      Option         "DPMS"
      Option         "Primary" "true"
   '';
    screenSection = ''
      Option         "metamodes" "DP-1: 1920x1080 +0+190, HDMI-0: nvidia-auto-select +1920+0 {rotation=right}"
      Option         "SLI" "Off"
      Option         "MultiGPU" "Off"
      Option         "BaseMosaic" "off"
    '';
  };

  # Enable WireGuard VPN
  # networking.wg-quick.interfaces = {
  #   wg0 = {
  #     address = [ "10.66.66.2/32" "fd42:42:42::2/128" ];
  #     dns = [ "94.140.14.14" "94.140.15.15" ];
  #     privateKey = secrets.wireguard.privateKey;
  #     listenPort = 54635;

  #     peers = [{
  #       publicKey = "YlsWHqCsU2jE+bMnzFazyZG6u4dSjuTwy621VbDpAxI=";
  #       presharedKey = secrets.wireguard.presharedKey;
  #       allowedIPs = [ "0.0.0.0/0" "::/0" ];
  #       endpoint = secrets.wireguard.endpoint;
  #       persistentKeepalive = 25;
  #     }];
  #   };
  # };
}
