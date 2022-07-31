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

# Installation

The "easy" way will only work on Arch out of the box.

After cloning this repo, move to the root of this repo and install the build
dependency packages:

```
pacman -S --needed - < make_pkgs
```

Build/install xmonad/xmobar binaries:

```
stack install
```

Install official runtime dependencies:

```
pacman -S --needed $(./scripts/pacman_deps)
```

Install unofficial runtime dependencies with your favorite AUR helper (which is
obviously yay):

```
yay -S $(./scripts/aur_deps)
```
