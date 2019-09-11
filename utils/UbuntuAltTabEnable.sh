#!/bin/bash

keySwitchApplication="switch-applications"
keySwitchApplicationBackward="switch-applications-backward"

backupSwitchApplications="$(gsettings get org.gnome.desktop.wm.keybindings "$keySwitchApplication")"
disableSwitchApplications="$(gsettings get org.gnome.desktop.wm.keybindings "$keySwitchApplication" | sed "s/\,*\s*'<Alt>Tab'//")"

backupSwitchApplicationsBackward="$(gsettings get org.gnome.desktop.wm.keybindings "$keySwitchApplicationBackward")"
disableSwitchApplicationsBackward="$(gsettings get org.gnome.desktop.wm.keybindings "$keySwitchApplicationBackward" | sed "s/\,*\s*'<Shift><Alt>Tab'//")"

disabled="0"

while true; do
  isActive=$(wmctrl -lx | awk -v search=$(printf 0x0%x $(xdotool getactivewindow)) -v wm_class="$wm_class" '{ if($1 ~ search && $3 ~ /Godot/) print $3 }')

  if [[ "$isActive" != "" ]]; then
    # echo "active"
    if [[ "$disabled" == "0" ]]; then
      # echo "disable shortcut"
      gsettings set org.gnome.desktop.wm.keybindings "$keySwitchApplication" "$disableSwitchApplications"
      gsettings set org.gnome.desktop.wm.keybindings "$keySwitchApplicationBackward" "$disableSwitchApplicationsBackward"
      disabled="1";
    fi
  else
    # echo "not active"
    if [[ "$disabled" == "1" ]]; then
      # echo "enable shortcut"
      gsettings set org.gnome.desktop.wm.keybindings "$keySwitchApplication" "$backupSwitchApplications"
      gsettings set org.gnome.desktop.wm.keybindings "$keySwitchApplicationBackward" "$backupSwitchApplicationsBackward"
      disabled="0"
    fi;
  fi;
  sleep 1
done