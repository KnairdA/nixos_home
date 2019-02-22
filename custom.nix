{ pkgs, ... }:

{
  options.custom = {
    hidpi = pkgs.lib.mkOption {
      type = pkgs.lib.types.bool;
      description = "Configure UI for high DPI displays";
    };

    tasks = pkgs.lib.mkOption {
      type = pkgs.lib.types.attrs;
    };
  };
}
