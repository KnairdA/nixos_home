{ pkgs, ... }:

{
  programs.fish = {
    enable = true;

    shellAliases = {
      ns  = "persistent-nix-shell -s";
      cat = "bat --plain";
    };

    shellInit = ''
      set fish_greeting ""
    '';

    interactiveShellInit = ''
      eval (dircolors -c ${./conf/dir_colors})

      set fish_color_normal         white
      set fish_color_command        magenta --bold
      set fish_color_param          green
      set fish_color_error          brred
      set fish_color_operator       cyan
      set fish_color_comment        white
      set fish_color_cwd            green
      set fish_color_quote          brown
      set fish_color_autosuggestion 555
      set fish_color_redirection    white

      function fish_prompt
        if test $TERM = "dumb"
          echo "\$ "
          return 0
        end

        set last_status $status

        if not set -q __fish_prompt_normal
          set -g __fish_prompt_normal (set_color normal)
        end

        switch $USER
          case root
            if not set -q __fish_prompt_cwd
              if set -q fish_color_cwd_root
                set -g __fish_prompt_cwd (set_color $fish_color_cwd_root)
              else
                set -g __fish_prompt_cwd (set_color $fish_color_cwd)
              end
            end

          case '*'
            if not set -q __fish_prompt_cwd
              set -g __fish_prompt_cwd (set_color $fish_color_cwd)
            end
        end

        echo -n -s 'λ ' "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal"

        if test $IN_NIX_SHELL
          if test $name
            echo -n -s ' (' $name ')'
          else
            echo -n -s ' (nix-shell)'
          end
        end

        if test $last_status -gt 0
          set_color $fish_color_cwd_root
          echo -n -s ' • '
          set_color normal
        else
          echo -n -s ' • '
        end
      end

      function fish_right_prompt
        set_color $fish_color_autosuggestion
        date '+%H:%M:%S'
        set_color normal
      end
    '';
  };
}
