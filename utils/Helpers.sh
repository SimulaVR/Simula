# The following functions assume they are called from project root.

updateEmail() {
    if [ -e $SIMULA_CONFIG_DIR/email ]; then
        # .. do nothing ..
        echo ""
    else
        dialog --title "SimulaVR" --backtitle "OPTIONAL: Provide email for important Simula updates & improved bug troubleshooting" --inputbox "Email: " 8 60 --output-fd 1 > $SIMULA_CONFIG_DIR/email 2>&1
        curl --data-urlencode emailStr@email https://www.wolframcloud.com/obj/george.w.singer/emailMessage
        clear
    fi
}

# Idempotent function which forces the ./submodules/godot/bin/godot.x11.tools.64 binary's
# RPATH to point to our local wlroots (in ./submodules/wlroots) instead of some other
# wlroots in the nix store
patchGodotWlroots(){
    PATH_TO_SIMULA_WLROOTS="`pwd`/submodules/wlroots/build/"
    OLD_RPATH="`./result/bin/patchelf --print-rpath submodules/godot/bin/godot.x11.tools.64`"
    if [[ $OLD_RPATH != $PATH_TO_SIMULA_WLROOTS* ]]; then # Check if the current RPATH contains our local simula wlroots build. If not, patchelf it to add it
        echo "Patching godot.x11.tools to point to local wlroots lib"
        echo "Changing path to: $PATH_TO_SIMULA_WLROOTS:$OLD_RPATH"
        ./result/bin/patchelf --set-rpath "$PATH_TO_SIMULA_WLROOTS:$OLD_RPATH" submodules/godot/bin/godot.x11.tools.64
    else
        echo "Not patching godot.x11.tools, already patched."
    fi
}

# rr helper function
zenRR() {
   sudo python3 ./utils/zen_workaround.py
}

removeSimulaXDGFiles() {
    # Get current timestamp for backup files
    TIMESTAMP=$(date +"%Y-%m-%d-%H:%M")

    # Helper function to backup and remove a file
    backup_and_remove() {
        local file="$1"
        if [ -f "$file" ]; then
            read -p "Would you like to backup $file before deletion? (y/n) " answer
            case $answer in
                [Yy]* )
                    cp "$file" "${file}.${TIMESTAMP}.bak"
                    echo "Backup created at ${file}.${TIMESTAMP}.bak"
                    ;;
                * )
                    echo "No backup created"
                    ;;
            esac
            rm -f "$file"
            echo "Removed $file"
        fi
    }

    # Helper function to backup and remove a directory
    backup_and_remove_dir() {
        local dir="$1"
        if [ -d "$dir" ]; then
            read -p "Would you like to backup the $(basename "$dir") directory before deletion? (y/n) " answer
            case $answer in
                [Yy]* )
                    cp -r "$dir" "${dir}.${TIMESTAMP}.bak"
                    echo "Backup created at ${dir}.${TIMESTAMP}.bak"
                    ;;
                * )
                    echo "No backup created"
                    ;;
            esac
            rm -rf "$dir"
            echo "Removed $(basename "$dir") directory"
        fi
    }

    # Set default XDG paths if not set
    : "${XDG_DATA_HOME:=$HOME/.local/share}"
    : "${XDG_CONFIG_HOME:=$HOME/.config}"
    : "${XDG_CACHE_HOME:=$HOME/.cache}"

    # Set `SIMULA_*` directories
    : "${SIMULA_DATA_DIR:=$XDG_DATA_HOME/Simula}"
    : "${SIMULA_CONFIG_DIR:=$XDG_CONFIG_HOME/Simula}"
    : "${SIMULA_CACHE_DIR:=$XDG_CACHE_HOME/Simula}"

    # Backup/remove config files
    backup_and_remove "$SIMULA_CONFIG_DIR/HUD.config"
    backup_and_remove "$SIMULA_CONFIG_DIR/config.dhall"
    backup_and_remove "$SIMULA_CONFIG_DIR/email"
    backup_and_remove "$SIMULA_CONFIG_DIR/UUID"

    # Backup/remove config files
    if [ -d "$SIMULA_DATA_DIR/log" ]; then
        for logfile in "$SIMULA_DATA_DIR/log"/*; do
            if [ -f "$logfile" ]; then
                backup_and_remove "$logfile"
            fi
        done
    fi

    # Backup/remove environments and media directories
    backup_and_remove_dir "$SIMULA_DATA_DIR/environments"
    backup_and_remove_dir "$SIMULA_DATA_DIR/media"

    echo "Simula XDG_* files have been cleared"
}