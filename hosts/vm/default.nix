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
      # media = {
        # daw.enable = true;
        # documents.enable = true;
        # graphics.enable = true;
        # mpv.enable = true;
        # recording.enable = true;
        # spotify.enable = true;
      # };
      term = {
        default = "alacritty";
	alacritty.enable = true;
      };
      # vm = {
      #   # qemu.enable = true;
      # };
    };
    editors = {
      default = "nvim";
      # emacs.enable = true;
      vim.enable = true;
    };
    shell = {
      # adl.enable = true;
      # bitwarden.enable = true;
      # direnv.enable = true;
      git.enable    = true;
      # gnupg.enable  = true;
      # tmux.enable   = true;
      zsh.enable    = true;
    };
    services = {
      ssh.enable = true;
    };
    theme.active = "doom-one";
  };

  ## Local config
  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;
  networking.networkmanager.enable = true;
}
