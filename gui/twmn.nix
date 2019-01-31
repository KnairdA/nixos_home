{ pkgs, ... }:

{
  home.packages = with pkgs; [
    twmn
    libnotify
  ];

  systemd.user.services.twmnd = {
    Unit = {
      Description = "twmn notification deamon";
      After  = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };

    Service = {
      Environment = "PATH=%h/.nix-profile/bin";
      ExecStart   = "${pkgs.twmn}/bin/twmnd";
      Restart     = "on-failure";
    };
  };

  home.file.".config/twmn/twmn.conf".text = ''
    [main]
    duration=5000
    [gui]
    always_on_top=true
    background_color=#909737
    bounce=true
    font=Iosevka
    foreground_color=#111111
    in_animation=6
    in_animation_duration=500
    opacity=100
    out_animation=5
    out_animation_duration=500
    position=bottom_right
    screen=0
    font_size=15
    height=20
  '';
}
