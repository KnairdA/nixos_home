from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget

from typing import List

mod = "mod4"

def hide_show_bar(qtile):
    bar = qtile.currentScreen.top
    if bar.is_show():
        bar.show(is_show=False)
    else:
        bar.show(is_show=True)
    qtile.currentGroup.layoutAll()

keys = [
    Key([mod], "k", lazy.layout.down()),
    Key([mod], "j", lazy.layout.up()),

    Key([mod, "shift"], "k", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_up()),

    Key([mod], "l", lazy.layout.next()),
    Key([mod], "h", lazy.layout.previous()),

    Key([mod], "s", lazy.layout.toggle_split()),

    Key([mod], "Return", lazy.spawn("kitty")),

    Key([mod], "v", lazy.next_layout()),

    Key([mod, "shift"], "q", lazy.window.kill()),

    Key(["control", "mod1"], "Escape",    lazy.shutdown()),
    Key(["control", "mod1"], "BackSpace", lazy.restart()),

    Key([mod], "space", lazy.spawn("rofi -show combi")),

    Key([mod, "shift"], "b", lazy.function(hide_show_bar)),
]

groups = [Group(i) for i in "0123456789"]

for i in groups:
    keys.extend([
        Key([mod], i.name, lazy.group[i.name].toscreen()),
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
    ])

layouts = [
    layout.Max(),
    layout.Stack(
        num_stacks=2,
        border_focus="#909636",
        border_normal="#161616",
        border_width=5
    ),
    layout.Stack(
        num_stacks=3,
        border_focus="#909636",
        border_normal="#161616",
        border_width=5
    ),
]

widget_defaults = dict(
    font='Iosevka',
    fontsize=22,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    foreground = '#161616',
                    active = '#161616',
                    inactive = '#161616',
                    disable_drag = True,
                    this_current_screen_border = '#161616',
                ),
                widget.WindowName(
                    foreground = '#161616',
                ),
                widget.Systray(
                    icon_size  = 25,
                ),
                widget.Clock(
                    foreground = '#161616',
                    format = '%Y-%m-%d %R'
                ),
            ],
            38,
            background = "#909636",
        ),
    ),
]

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = True
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"
