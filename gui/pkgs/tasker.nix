{ pkgs, tasks, ... }:

pkgs.lib.mapAttrsToList (name: conf: let
  command = pkgs.writeTextFile {
    name        = "tasker_cmd_" + name;
    executable  = true;
    destination = "/bin/tasker_cmd_" + name;
    text = pkgs.lib.attrByPath [ conf.type ] "" {
      terminal = ''
        #!/bin/sh
        exec ${pkgs.kitty}/bin/kitty -d ${conf.directory} ${conf.command}
      '';
      launcher = ''
        #!/bin/sh
        pushd ${conf.directory}
          exec ${conf.command}
        popd
      '';
    };
  };
  shortcut = pkgs.writeTextFile {
    name        = "tasker_shortcut_" + name;
    executable  = false;
    destination = "/share/applications/tasker_shortcut_" + name + ".desktop";
    text = ''
      [Desktop Entry]
      Type=Application
      Name=${conf.description}
      GenericName=Tasker
      Exec=${command}/bin/tasker_cmd_${name}
      Terminal=false
    '';
  };
in pkgs.symlinkJoin {
  name = "tasker_task_" + name;
  paths = [ shortcut ];
}) tasks
