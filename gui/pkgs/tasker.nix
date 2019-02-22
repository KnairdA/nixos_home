{ pkgs, tasks, ... }:

pkgs.lib.mapAttrsToList (name: value: let
  command = pkgs.writeTextFile {
    name        = "tasker_cmd_" + name;
    executable  = true;
    destination = "/bin/tasker_cmd_" + name;
    text = ''
      #!/bin/sh
      pushd ${toString value.directory}
        exec ${toString value.command}
      popd
    '';
  };
  shortcut = pkgs.writeTextFile {
    name        = "tasker_shortcut_" + name;
    executable  = false;
    destination = "/share/applications/tasker_shortcut_" + name + ".desktop";
    text = ''
      [Desktop Entry]
      Type=Application
      Name=${toString value.description}
      Exec=${command}/bin/tasker_cmd_${toString name}
      Terminal=false
    '';
  };
in pkgs.symlinkJoin {
  name = "tasker_task_" + name;
  paths = [ shortcut ];
}) tasks
