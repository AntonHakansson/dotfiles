{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.emacs;
    configDir = config.dotfiles.configDir;
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    doom = {
      enable  = mkBoolOpt true;
      fromSSH = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs; [
      ## Emacs itself
      binutils       # native-comp needs 'as', provided by this
      ((emacsPackagesNgGen emacsPgtkGcc).emacsWithPackages (epkgs: [ epkgs.vterm ]))

      ## Doom dependencies
      git
      (ripgrep.override {withPCRE2 = true;})
      gnutls              # for TLS connectivity

      ## Optional dependencies
      fd                  # faster projectile indexing
      imagemagick         # for image-dired
      (mkIf (config.programs.gnupg.agent.enable)
        pinentry_emacs)   # in-emacs gnupg prompts
      zstd                # for undo-fu-session/undo-tree compression

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [
        en en-computers en-science sv
      ]))
      # :checkers grammar
      languagetool
      # :tools editorconfig
      editorconfig-core-c # per-project style config
      # :tools lookup & :lang org +roam
      sqlite

      # :lang nix
      nixfmt
      # lang sh
      shellcheck
      shfmt
      # :lang cc
      ccls
      # :lang javascript
      nodePackages.javascript-typescript-langserver
      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-full
      # :lang rust
      rustfmt
      unstable.rust-analyzer
      # lang markdown
      pandoc
      # lang python
      (python39.withPackages (ps: with ps; [
        jupyter
        ipykernel jupyterlab
        matplotlib numpy pandas seaborn
        networkx
      ]))
      # lang org
      graphviz
      gnuplot
      drawio
    ];

    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];
    environment.variables.EMACSDIR = "$XDG_CONFIG_HOME/emacs";
    environment.variables.DOOMDIR = "$XDG_CONFIG_HOME/doom";

    modules.shell.zsh.rcFiles = [ "${configDir}/emacs/aliases.zsh" ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # init.doomEmacs = mkIf cfg.doom.enable ''
    #   if [ -d $HOME/.config/emacs ]; then
    #      ${optionalString cfg.doom.fromSSH ''
    #         git clone git@github.com:hlissner/doom-emacs.git $HOME/.config/emacs
    #      ''}
    #      ${optionalString (cfg.doom.fromSSH == false) ''
    #         git clone https://github.com/hlissner/doom-emacs $HOME/.config/emacs
    #      ''}
    #   fi
    # '';
  };
}
 
