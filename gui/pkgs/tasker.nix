{ stdenv, pkgs, ... }:

pkgs.writeTextFile {
  name        = "tasker";
  executable  = true;
  destination = "/bin/tasker";

  text = with pkgs; ''
    #!${fish}/bin/fish

    pushd ~/.local/share/tasks
    set task (find . -executable -type f | cut -c3- | rofi -dmenu -p "task")

    if test $status -eq 0
        eval $task
    end
    popd
  '';
}
