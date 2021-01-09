{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
  ];

  custom.hidpi = false;

  custom.tasks = {
    "compustream_shell" = {
      description = "compustream dev shell";
      directory = "~/projects/dev/compustream";
      type = "local-shell";
    };
  };

  services.kdeconnect = {
    enable = true;
  };
}
