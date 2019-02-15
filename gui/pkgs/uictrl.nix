{ stdenv, pkgs, ... }:

pkgs.writeTextFile {
  name        = "uictrl";
  executable  = true;
  destination = "/bin/uictrl";

  text = with pkgs; ''
    #!${fish}/bin/fish

    function cycle
        set nsp_ws     (${wmctrl}/bin/wmctrl -d | awk '{if ($9 == "NSP") {print $1}}')
        set current_ws (${wmctrl}/bin/wmctrl -d | awk '{if ($2 == "*")   {print $1}}')

        ${wmctrl}/bin/wmctrl -l | awk -v current_ws=$current_ws -v nsp_ws=$nsp_ws '{if ($2 > current_ws && $2 != nsp_ws) {print $2}}' | uniq
        ${wmctrl}/bin/wmctrl -l | awk -v current_ws=$current_ws -v nsp_ws=$nsp_ws '{if ($2 < current_ws && $2 != nsp_ws) {print $2}}' | uniq
    end

    switch $argv[1]
        case next
            ${wmctrl}/bin/wmctrl -s (cycle)[1]
        case prev
            ${wmctrl}/bin/wmctrl -s (cycle | tac)[1]
    end
  '';
}
