{ pkgs, ... }:

{
  programs.autorandr = {
    enable = true;

    profiles = {
      "mobile" = {
        fingerprint = {
          LVDS1 = "00ffffffffffff0030e4d3020000000000150103801c1078ea10a59658578f2820505400000001010101010101010101010101010101381d56d45000163030202500159c1000001b000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d544c423100f7";
        };
        config = {
          LVDS1 = {
            enable   = true;
            primary  = true;
            mode     = "1366x768";
            position = "0x0";
          };
        };
      };
      "docked" = {
        fingerprint = {
          LVDS1 = "00ffffffffffff0030e4d3020000000000150103801c1078ea10a59658578f2820505400000001010101010101010101010101010101381d56d45000163030202500159c1000001b000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d544c423100f7";
          HDMI3 = "00ffffffffffff0010ac0d404a574841020f010380261f78eeee95a3544c99260f5054a54b00714f8180010101010101010101010101302a009851002a4030701300782d1100001e000000ff0054363131373531344148574a0a000000fc0044454c4c203139303546500a20000000fd00384c1e510e000a20202020202000e6";
        };
        config = {
          LVDS1 = {
            enable   = true;
            primary  = true;
            mode     = "1366x768";
            position = "0x256";
          };
          HDMI3 = {
            enable   = true;
            mode     = "1280x1024";
            position = "1372x0";
          };
        };
      };
    };
  };

  xsession.initExtra = "${pkgs.autorandr}/bin/autorandr --change";
}
