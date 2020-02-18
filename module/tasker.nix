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
  launchJupyterInDirectory = dir: env: ''
    #!/usr/bin/env nix-shell
    #!nix-shell ${builtins.unsafeDiscardStringContext env.drvPath} -i fish

    for port in (seq 9000 9100)
      if not ss --listening --oneline --tcp --no-header | awk '{ split($4, port, ":"); print port[2]+0 }' | grep -q $port
        set free_port $port
        break
      end
    end

    set token (head /dev/urandom | tr -dc A-Za-z0-9 | head -c 40)
    jupyter lab --no-browser --port=$free_port --NotebookApp.token=$token &
    sleep 2
    ${pkgs.chromium}/bin/chromium --app="http://localhost:$free_port/?token=$token"
    kill (jobs -lp)
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

        python-console = launchCommandInDirectory "~/" ''
          nix-shell ${builtins.unsafeDiscardStringContext conf.environment.drvPath} --command jupyter-qtconsole
        '';

        jupyter-lab = launchJupyterInDirectory conf.directory conf.environment;
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
    paths = [ command shortcut ];
  }) config.custom.tasks;

in {
  home.packages = taskivations;
}
