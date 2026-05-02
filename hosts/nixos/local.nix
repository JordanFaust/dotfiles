{
  config,
  lib,
  ...
}:
with lib; {
  time.timeZone = mkDefault "America/Chicago";
  i18n.defaultLocale = mkDefault "en_US.UTF-8";
  # For redshift, mainly
  location = {
    latitude = 39.035300;
    longitude = -94.463600;
  };
}
