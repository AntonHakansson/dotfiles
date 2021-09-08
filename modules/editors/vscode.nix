{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.vscode;
  extensions = with pkgs.vscode-extensions;
    [
      vscodevim.vim
      ms-vsliveshare.vsliveshare

      # nix
      bbenoist.nix

      # typescript
      ms-vscode.cpptools

      # typescript
      jpoissonnier.vscode-styled-components
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
      # One Dark Pro
      name = "Material-theme";
      publisher = "zhuangtongfa";
      version = "3.11.4";
      sha256 = "sha256-3jo5oToo3Lk94Z5VW/zWCWv1GcCUTC3F4fI+erD0SDI=";
    }];
  vscode-with-extensions = (pkgs.unstable.vscode-with-extensions.override {
    vscodeExtensions = extensions;
  });
in {

  options.modules.editors.vscode = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ vscode-with-extensions ];
    home.configFile."Code/User/nixos-settings.json".text = ''
      {
        "editor.fontFamily": "Fira Code",
        "window.menuBarVisibility": "toggle",
        "workbench.colorTheme": "One Dark Pro",
        "vim.useSystemClipboard": true
      }
    '';

    system.userActivationScripts = mkIf cfg.enable {
      vscodeMergeConfigs = let
        nixos-config = "$XDG_CONFIG_HOME/Code/User/nixos-settings.json";
        code-config = "$XDG_CONFIG_HOME/Code/User/settings.json";
      in ''
        if [ -e ${code-config} ]
        then
          merged_settings=$(${pkgs.jq}/bin/jq -s '.[0] * .[1]' ${nixos-config} ${code-config})
          $(echo $merged_settings | ${pkgs.jq}/bin/jq) && $(echo $merged_settings > ${code-config})
        else
          cp ${nixos-config} ${code-config}
          chmod +w ${code-config}
        fi
      '';
    };
  };
}
