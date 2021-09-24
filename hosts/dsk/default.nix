{ pkgs, config, lib, ... }:
let secrets = import ./secrets/secrets.nix;
in
{
  imports = [ ../home.nix ./hardware-configuration.nix ];

  ## Modules
  modules = {
    desktop = {
      awesomewm.enable = true;
      apps = {
        discord.enable = true;
        zoom.enable = true;
        rofi.enable = true;
        unity3d.enable = true;
        flameshot.enable = true;
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
}
