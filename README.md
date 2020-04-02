# XMonad-config

Built just for me...although you may fork if you like it ;)

# Features

* ACPI event listener: reacts to events from acpid such as power button presses
  and lid locks
* Smart dynamic Workspace manager: spawns and destroys app-specific workspaces
  on the fly
* Xmobar: spawns a child Xmobar process with unified theme
* DBus integration: commands such as brightness and screensaver control emit
  events that can be captured (currently used to control indicators on XMobar)
* Rofi integration: uses rofi in various forms for:
   * launching apps
   * selecting windows
   * viewing keybindings
   * selecting Wifi networks (networkmanager_dmenu)
   * clipboard management (greenclip)
   * mounting disks
