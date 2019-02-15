{ pkgs, ... }:

{
  home.file.".config/touchegg/touchegg.conf".lines = let
    uictrl = pkgs.lib.callPackageWith pkgs ./pkgs/uictrl.nix { };
  in ''
    <touchégg>
    <application name="All">
    <gesture type="DRAG" fingers="3" direction="LEFT">
        <action type="RUN_COMMAND">${uictrl}/bin/uictrl prev</action>
    </gesture>
    <gesture type="DRAG" fingers="3" direction="RIGHT">
        <action type="RUN_COMMAND">${uictrl}/bin/uictrl next</action>
    </gesture>
    <gesture type="TAP" fingers="2">
        <action type="MOUSE_CLICK">BUTTON=3</action>
    </gesture>
    </application>
    </touchégg>
  '';

  systemd.user.services.touchegg = {
    Unit = {
      Description = "Touchégg multitouch gestures";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${pkgs.touchegg}/bin/touchegg";
      Restart = "on-failure";
    };
  };
}
