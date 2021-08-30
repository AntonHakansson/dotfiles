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
        zoom.enable = true;
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
}
