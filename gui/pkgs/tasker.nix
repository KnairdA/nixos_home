{ pkgs, tasks, ... }:

pkgs.lib.mapAttrsToList (name: conf: let

  command = pkgs.writeTextFile {
    name        = "tasker_cmd_" + name;
    executable  = true;
    destination = "/bin/tasker_cmd_" + name;
    text = pkgs.lib.attrByPath [ conf.type ] "" {
      launcher = ''
        #!/bin/sh
        pushd ${conf.directory}
          exec ${conf.command}
        popd
      '';
      terminal = ''
        #!/bin/sh
        exec ${pkgs.kitty}/bin/kitty -d ${conf.directory} ${conf.command}
      '';
      local-shell = ''
        #!/bin/sh
        exec ${pkgs.kitty}/bin/kitty -d ${conf.directory} nix-shell --command fish
      '';
      local-editor = ''
        #!/bin/sh
        pushd ${conf.directory}
          exec nix-shell --run 'nvim-qt --no-ext-tabline'
        popd
      '';
      environment = ''
        #!/bin/sh
        exec ${pkgs.kitty}/bin/kitty -d ${conf.directory} nix-shell \
             ${builtins.unsafeDiscardStringContext conf.environment.drvPath} --command fish
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
