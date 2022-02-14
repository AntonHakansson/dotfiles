{ config, options, pkgs, lib, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.vpn;
in {
  options.modules.services.vpn = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.openvpn.servers = let
      resources = pkgs.fetchzip {
        name = "pia-vpn-config";
        url = "https://www.privateinternetaccess.com/openvpn/openvpn.zip";
        sha256 = "ZA8RS6eIjMVQfBt+9hYyhaq8LByy5oJaO9Ed+x8KtW8=";
        stripRoot = false;
      };
      fixup = (builtins.replaceStrings [ ".ovpn" "_" ] [ "" "-" ]);
      servers =
        (builtins.filter (name: !(isNull (builtins.match ".+ovpn$" name)))
          (builtins.attrNames (builtins.readDir resources)));
      make_server = (name: {
        name = fixup name;
        value = {
          autoStart = false;
          # FIXME:
          # authUserPass = config.services.pia.authUserPass;
          # no checkin
          # authUserPass = {
          #   username = "xxxxxxxx";
          #   password = "xxxxxxxxxxxxxx";
          # };
          config = "config ${resources}/${name}";
          updateResolvConf = true;
        };
      });
    in builtins.listToAttrs (map make_server servers);
  };
}
