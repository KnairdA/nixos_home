{ config, pkgs, ... }:

let
  emumenu = pkgs.writeTextFile {
    name        = "emumenu";
    executable  = true;
    destination = "/bin/dmenu-wl";

    text = ''
      #!/usr/bin/env bash
      ${pkgs.rofi}/bin/rofi -dmenu -p "pass" "$@"
    '';
  };

  passrofi = pkgs.writeTextFile {
    name        = "passrofi";
    executable  = true;
    destination = "/bin/passrofi";

    text = ''
      #!/usr/bin/env bash
      export PATH="$PATH:${emumenu}/bin"
      ${pkgs.pass}/bin/.passmenu-wrapped
    '';
  };

in {
  home.packages = [
    pkgs.pass
    passrofi
  ];
}
