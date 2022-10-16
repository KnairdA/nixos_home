{ pkgs, ... }:

let
  mkOption = pkgs.lib.mkOption;
  types = pkgs.lib.types;

in {
  options.custom = {
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
            type = types.enum [ "launcher" "terminal" "local-shell" "local-editor" "environment" "python-console" "jupyter-lab" ];
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

    notebooks = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          description = mkOption {
            type = types.uniq types.string;
          };
          environment = mkOption {
            type = types.package;
          };
        };
      });
    };
  };
}
