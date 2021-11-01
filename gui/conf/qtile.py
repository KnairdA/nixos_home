from typing import List

from libqtile import layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown, KeyChord
from libqtile.lazy import lazy

mod = "mod4"

keys = [
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),

    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),

    Key([mod], "l", lazy.layout.next()),
    Key([mod], "h", lazy.layout.previous()),

    Key([mod, "shift"], "Up",   lazy.layout.grow_up()),
    Key([mod, "shift"], "Down", lazy.layout.grow_down()),
    Key([mod, "shift"], "Left", lazy.layout.grow_left()),
    Key([mod, "shift"], "Right", lazy.layout.grow_right()),


    Key([mod], "Return", lazy.spawn("kitty")),
    Key([mod, "shift"], "Return", lazy.spawn("emacsclient --create-frame")),

    KeyChord([mod], "s", [
        Key([], "s", lazy.layout.toggle_split()),
        Key([], "j", lazy.next_layout()),
    ]),

    Key([mod, "shift"], "q", lazy.window.kill()),

    Key(["control", "mod1"], "Escape",    lazy.shutdown()),
    Key(["control", "mod1"], "BackSpace", lazy.restart()),

    Key(["control", "mod1"], "l", lazy.spawn("i3lock -c 000000")),
    Key([], "Print", lazy.spawn("flameshot gui")),

    Key([mod], "space", lazy.spawn("rofi -show combi")),
    Key([mod], "p", lazy.spawn("passrofi")),

    Key([], "Menu", lazy.group['scratch'].dropdown_toggle('term')),
    Key([mod], "b",  lazy.group['scratch'].dropdown_toggle('browser')),
    Key([mod], "m",  lazy.group['scratch'].dropdown_toggle('telegram')),

    KeyChord([mod], "c", [
        Key([], "m",    lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")),
        Key([], "Up",   lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +10%")),
        Key([], "Down", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -10%")),
        Key([], "s",    lazy.spawn("systemctl suspend")),
        Key([], "h",    lazy.spawn("systemctl hibernate")),
    ]),
]

groups = [Group(i) for i in "1234567890"]

for i in groups:
    keys.extend([
        Key([mod], i.name, lazy.group[i.name].toscreen()),
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
    ])

scratchpad_defaults = {
    "height": 0.8,
    "width": 0.9,
    "x": 0.05,
    "y": 0.0,
    "on_focus_lost_hide": False,
}

scratchpad_rhs = {
    "height": 0.8,
    "width": 0.3,
    "x": 0.7,
    "y": 0.1,
    "on_focus_lost_hide": False,
}

groups += [
    ScratchPad('scratch', [
        DropDown("term", "kitty", **scratchpad_defaults),
        DropDown("browser", "firefox", **scratchpad_defaults),
        DropDown("telegram", "telegram-desktop", **scratchpad_rhs)
    ]),
]

border_default = {
    "border_focus" : "#909636",
    "border_normal": "#161616",
    "border_width" : 2,
}

layouts = [
    layout.Max(),
    layout.MonadTall(**border_default),
    layout.Stack(num_stacks=2, **border_default),
    layout.Stack(num_stacks=3, **border_default),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

screens = [
    Screen()
]

floating_layout = layout.Floating(
    border_focus="#aadb0f",
    border_normal="#161616",
    border_width=4,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class='ssh-askpass'),  # ssh-askpass
        Match(title='pinentry'),  # GPG key password entry
    ])

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

wmname = "LG3D"
