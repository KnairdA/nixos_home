{ config, pkgs, ... }:

{
  programs.htop = {
    enable = true;

    settings = {
      treeView = true;

      showProgramPath = false;
      highlightBaseName = true;

      hideThreads         = true;
      hideUserlandThreads = true;

      left_meter_modes = [ "LeftCPUs4" "Memory" "Swap" ];
      right_meter_modes = [ "RightCPUs4" "Tasks" "LoadAverage" "Uptime" ];
    };
  };
}
