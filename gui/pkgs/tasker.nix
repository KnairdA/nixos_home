{ pkgs, tasks, ... }:

pkgs.lib.mapAttrsToList (name: value: let
  command = pkgs.writeTextFile {
    name        = "tasker_cmd_" + name;
    executable  = true;
    destination = "/bin/tasker_cmd_" + name;
    text = if value.terminal then ''
      #!/bin/sh
      exec ${pkgs.kitty}/bin/kitty -d ${value.directory} ${value.command}
    '' else ''
      #!/bin/sh
      pushd ${value.directory}
        exec ${value.command}
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
      Name=${value.description}
      GenericName=Tasker
      Exec=${command}/bin/tasker_cmd_${name}
      Terminal=false
    '';
  };
in pkgs.symlinkJoin {
  name = "tasker_task_" + name;
  paths = [ shortcut ];
}) tasks
