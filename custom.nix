{ pkgs, ... }:

let
  mkOption = pkgs.lib.mkOption;
  types = pkgs.lib.types;
in {
  options.custom = {
    hidpi = mkOption {
      type = types.bool;
      description = "Configure UI for high DPI displays";
    };

    tasks = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          description = mkOption {
            type = types.uniq types.string;
          };
          directory = mkOption {
            type = types.str;
            default = "~/";
          };
          type = mkOption {
            type = types.enum [ "launcher" "terminal" "local-shell" "local-editor" "environment" ];
            default = "launcher";
          };
          command = mkOption {
            type = types.str;
          };
          environment = mkOption {
            type = types.package;
          };
        };
      });
    };
  };
}
