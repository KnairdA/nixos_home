{ config, pkgs, ... }:

{
  programs.htop = {
    enable = true;

    treeView = true;
    showProgramPath = false;
    highlightBaseName = true;
    hideThreads = true;
    hideUserlandThreads = true;
    meters.left = [ "LeftCPUs4" "Memory" "Swap" ];
    meters.right = [ "RightCPUs4" "Tasks" "LoadAverage" "Uptime" ];
  };
}
