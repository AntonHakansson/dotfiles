{ config, lib, ... }:

with lib; {
  ## Location config
  time.timeZone = mkDefault "Europe/Stockholm";
  i18n.defaultLocale = mkDefault "en_US.UTF-8";
  # For redshift, mainly
  location = (if config.time.timeZone == "Europe/Stockholm" then {
    latitude = 59.2;
    longitude = 18.03;
  } else
    { });
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "altgr-intl";
}
