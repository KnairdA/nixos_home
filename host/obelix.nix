{ pkgs, ... }:

{
  imports = [
    ../gui/default.nix
    ../gui/xmonad.nix
  ];

  custom.tasks = {
    "compustream_shell" = {
      description = "compustream dev shell";
      directory = "~/projects/dev/compustream";
      type = "local-shell";
    };
  };
}
