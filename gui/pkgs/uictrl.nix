{ stdenv, pkgs, ... }:

pkgs.writeTextFile {
  name        = "uictrl";
  executable  = true;
  destination = "/bin/uictrl";

  text = builtins.readFile ./bin/uictrl.sh;
}
