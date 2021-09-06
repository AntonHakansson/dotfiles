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
    home.configFile = {
      "Code/User/settings.json".text = ''
        {
          "window.menuBarVisibility": "toggle"
          "workbench.colorTheme": "One Dark Pro"
        }
      '';
    };
  };
}
