{ pkgs, config, lib, ... }:
{
  imports = [
    ../home.nix
    ./hardware-configuration.nix
  ];

  ## Modules
  modules = {
    desktop = {
      bspwm.enable = true;
      apps = {
        # discord.enable = true;
        rofi.enable = true;
        # godot.enable = true;
        # signal.enable = true;
      };
      browsers = {
        default = "brave";
        brave.enable = true;
        firefox.enable = true;
        # qutebrowser.enable = true;
      };
      # gaming = {
        # steam.enable = true;
        # emulators.enable = true;
        # emulators.psx.enable = true;
      # };
      media = {
        # daw.enable = true;
        documents.enable = true;
        # graphics.enable = true;
        # mpv.enable = true;
        # recording.enable = true;
        # spotify.enable = true;
      };
      term = {
        default = "alacritty";
        alacritty.enable = true;
      };
      # vm = {
      #   # qemu.enable = true;
      # };
    };
    dev = {
      zig.enable = true;
    };
    editors = {
      default = "nvim";
      emacs.enable = true;
      vim.enable = true;
      # vscode.enable = true;
    };
    shell = {
      bitwarden.enable = true;
      direnv.enable = true;
      git.enable    = true;
      gnupg.enable  = true;
      # tmux.enable   = true;
      zsh.enable    = true;
    };
    services = {
      ssh.enable = true;
    };
    hardware = {
      audio.enable = true;
      nvidia.enable = true;
    };
    theme.active = "doom-one";
  };

  ## Local config
  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;
  networking.networkmanager.enable = true;

  # CPU
  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = "performance";

  # Monitors
  services.xserver = {
    # This must be done manually to ensure my screen spaces are arranged exactly
    # as I need them to be *and* the correct monitor is "primary". Using
    # xrandrHeads does not work.
    monitorSection = ''
      VendorName     "Unknown"
      ModelName      "Acer GN246HL"
      HorizSync       30.0 - 160.0
      VertRefresh     56.0 - 144.0
      Option         "DPMS"
    '';
    screenSection = ''
      Option         "Stereo" "0"
      Option         "metamodes" "DVI-I-1: 1920x1080_144 +0+225 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}, HDMI-0: nvidia-auto-select +1920+0 {rotation=right, ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}"
      Option         "SLI" "Off"
      Option         "MultiGPU" "Off"
      Option         "BaseMosaic" "off"
    '';
  };
}