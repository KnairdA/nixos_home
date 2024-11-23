{ pkgs, config, ... }:

let
  launchCommandInDirectory = dir: cmd: ''
    #!/bin/sh
    pushd ${dir}
      exec ${cmd}
    popd
  '';
  launchTerminalInDirectory = dir: cmd: ''
    #!/bin/sh
    exec ${pkgs.kitty}/bin/kitty -d ${dir} ${cmd}
  '';

  taskivations = pkgs.lib.mapAttrsToList (name: conf: let
    command = pkgs.writeTextFile {
      name        = "tasker_cmd_" + name;
      executable  = true;
      destination = "/bin/tasker_cmd_" + name;
      text = pkgs.lib.attrByPath [ conf.type ] "" {
        launcher = launchCommandInDirectory conf.directory conf.command;
        terminal = launchTerminalInDirectory conf.directory conf.command;

        local-shell  = launchTerminalInDirectory conf.directory "nix-shell --command fish";
        local-editor = launchCommandInDirectory  conf.directory "nix-shell --run 'nvim-qt --no-ext-tabline'";

        environment = launchTerminalInDirectory conf.directory ''
          nix-shell ${builtins.unsafeDiscardStringContext conf.environment.drvPath} --command fish
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
  }) config.custom.tasks;

in {
  home.packages = taskivations;
}
