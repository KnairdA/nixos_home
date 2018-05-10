{ pkgs, theme, ... }:

let
  config = with theme; ''
    #ifdef _WINDOW_CONFIG
      enum {
        WIN_WIDTH  = 800,
        WIN_HEIGHT = 600
      };
      static const char * const WIN_BG_COLOR = "${win_bg}";
      static const char * const BAR_FONT = "${bar_font_name}:size=${bar_font_size}";
      static const char * const WIN_FS_COLOR = "${win_fs}";
      static const char * const SEL_COLOR    = "${sel}";
      static const char * const BAR_BG_COLOR = "${bar_bg}";
      static const char * const BAR_FG_COLOR = "${bar_fg}";
    #endif
    #ifdef _IMAGE_CONFIG
      static const float zoom_levels[] = {
         12.5,  25.0,  50.0,  75.0,
        100.0, 150.0, 200.0, 400.0, 800.0
      };
      enum { SLIDESHOW_DELAY = 5 };
      static const double GAMMA_MAX = 10.0;
      static const int GAMMA_RANGE  = 32;
      static const int PAN_FRACTION = 5;
      static const bool ANTI_ALIAS  = false;
      static const bool ALPHA_LAYER = true;
    #endif
    #ifdef _THUMBS_CONFIG
      static const int thumb_sizes[] = { 32, 64, 96, 128, 160 };
      static const int THUMB_SIZE = 3;
    #endif
    #ifdef _MAPPINGS_CONFIG
      static const keymap_t keys[] = {
        { 0,            XK_q,             g_quit,               None },
        { 0,            XK_Return,        g_switch_mode,        None },
        { 0,            XK_f,             g_toggle_fullscreen,  None },
        { 0,            XK_b,             g_toggle_bar,         None },
        { 0,            XK_g,             g_first,              None },
        { 0,            XK_G,             g_n_or_last,          None },
        { 0,            XK_r,             g_reload_image,       None },
        { 0,            XK_plus,          g_zoom,               +1 },
        { 0,            XK_minus,         g_zoom,               -1 },
        { 0,            XK_m,             g_toggle_image_mark,  None },
        { 0,            XK_M,             g_reverse_marks,      None },
        { ControlMask,  XK_m,             g_unmark_all,         None },
        { 0,            XK_N,             g_navigate_marked,    +1 },
        { 0,            XK_P,             g_navigate_marked,    -1 },
        { 0,            XK_h,             t_move_sel,           DIR_LEFT },
        { 0,            XK_Left,          t_move_sel,           DIR_LEFT },
        { 0,            XK_j,             t_move_sel,           DIR_DOWN },
        { 0,            XK_Down,          t_move_sel,           DIR_DOWN },
        { 0,            XK_k,             t_move_sel,           DIR_UP },
        { 0,            XK_Up,            t_move_sel,           DIR_UP },
        { 0,            XK_l,             t_move_sel,           DIR_RIGHT },
        { 0,            XK_Right,         t_move_sel,           DIR_RIGHT },
        { 0,            XK_R,             t_reload_all,         None },
        { 0,            XK_n,             i_navigate,           +1 },
        { 0,            XK_n,             i_scroll_to_edge,     DIR_LEFT | DIR_UP },
        { 0,            XK_space,         i_navigate,           +1 },
        { 0,            XK_p,             i_navigate,           -1 },
        { 0,            XK_p,             i_scroll_to_edge,     DIR_LEFT | DIR_UP },
        { 0,            XK_BackSpace,     i_navigate,           -1 },
        { 0,            XK_h,             i_scroll,             DIR_LEFT },
        { 0,            XK_Left,          i_scroll,             DIR_LEFT },
        { 0,            XK_j,             i_scroll,             DIR_DOWN },
        { 0,            XK_Down,          i_scroll,             DIR_DOWN },
        { 0,            XK_k,             i_scroll,             DIR_UP },
        { 0,            XK_Up,            i_scroll,             DIR_UP },
        { 0,            XK_l,             i_scroll,             DIR_RIGHT },
        { 0,            XK_Right,         i_scroll,             DIR_RIGHT },
        { 0,            XK_equal,         i_set_zoom,           100 },
        { 0,            XK_a,             i_toggle_antialias,   None },
        { 0,            XK_A,             i_toggle_alpha,       None },
        { 0,            XK_s,             i_slideshow,          None },
      };
      static const button_t buttons[] = {
        { 0,            1,                i_cursor_navigate,    None },
        { 0,            3,                g_switch_mode,        None },
        { 0,            4,                g_zoom,               +1 },
        { 0,            5,                g_zoom,               -1 },
      };
    #endif
  '';
in pkgs.sxiv.override {
   conf = config;
}
