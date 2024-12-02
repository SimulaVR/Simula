# The following functions assume they are called from project root.

checkInstallNix() {
    if command -v nix; then
        echo "nix already installed.."
    else
        echo "nix not found..."
        echo "Please install nix from:"
        echo "https://nixos.org/download/"
    fi
}

checkInstallCachix() {
    if command -v cachix; then
        echo "cachix already installed.."
    else
        echo "cachix not found..."
        echo "Please install cachix. Example install command:"
        echo "nix-env -iA cachix -f https://cachix.org/api/v1/install"
    fi
}

checkInstallCurl() {
    if command -v curl; then
        echo "curl already installed.."
    else
        echo "curl not found..."
        echo "Please install curl. Example install command:"
        echo "nix-env -iA nixpkgs.curl"
    fi
}


checkInstallGit() {
    if command -v git; then
        echo "git already installed.."
    else
        nix-env -iA nixpkgs.git
        echo "curl not found..."
        echo "Please install curl. Example install command:"
        echo "nix-env -iA nixpkgs.git"
    fi
}

checkIfNixOS() {
    if [ -e /etc/NIXOS ]; then
        echo "true";
    else
        echo "false";
    fi
}

# devBuild helper function
switchToNix() {
    cd ./addons/godot-haskell-plugin
    rm -f libgodot-haskell-plugin.so
    ln -s ../../result/bin/libgodot-haskell-plugin.so libgodot-haskell-plugin.so
    cd -
}

# devBuild function
switchToLocal() {
    cd ./addons/godot-haskell-plugin
    rm -f libgodot-haskell-plugin.so
    path=$(nix-shell -Q shell.nix --run "../../result/bin/cabal list-bin flib:godot-haskell-plugin")
    ln -s "$path" libgodot-haskell-plugin.so
    cd -
}


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

# devBuild = true function
# => Updates godot-haskell to latest api.json generated from devBuildGodot
nsBuildGodotHaskell() {
  cd ./submodules/godot
  LD_LIBRARY_PATH=./modules/gdleapmotionV2/LeapSDK/lib/x64 ./bin/godot.x11.tools.64 --gdnative-generate-json-api ./bin/api.json
  cd -

  cd ./submodules/godot-haskell-cabal
  ./updateApiJSON.sh
  cd -
}

# devBuild = true function
nsBuildGodotHaskellPlugin() {
  cd ./addons/godot-haskell-plugin
  if [ -z $1 ]; then
    cabal build
  elif [ $1 == "--profile" ]; then
    cabal --enable-profiling build --ghc-options=\"-fprof-auto -rtsopts -fPIC -fexternal-dynamic-refs\"
  else
    while inotifywait -qqre modify .; do cabal build; done
  fi
  cd -
}

# devBuild = true function
nsREPLGodotHaskellPlugin() {
    cd ./addons/godot-haskell-plugin
    nix-shell shell.nix --run "cabal repl"
}

# devBuild = true function
# => Takes optional argument for a profile build
nsBuildSimulaLocal() {
    installSimula 1                      || { echo "installSimula 1 failed"; return 1; } # forces devBuild
    PATH=./result/bin:$PATH cabal update || { echo "cabal update failed"; return 1; }
    nsBuildMonado                        || { echo "nsBuildMonado failed"; return 1; }
    nsBuildWlroots                       || { echo "nsBuildWlroots failed"; return 1; }
    nsBuildGodot                         || { echo "nsBuildGodot failed"; return 1; }
    patchGodotWlroots                    || { echo "patchGodotWlroots failed"; return 1; }
    nsBuildGodotHaskell "$1"             || { echo "nsBuildGodotHaskell failed"; return 1; }
    nsBuildGodotHaskellPlugin "$1"       || { echo "nsBuildGodotHaskellPlugin failed"; return 1; }
    switchToLocal                        || { echo "switchToLocal failed"; return 1; }
}

# devBuild = true function
# => Patch our Godot executable to point to our local build of wlroots
patchGodotWlroots(){
    PATH_TO_SIMULA_WLROOTS="`pwd`/submodules/wlroots/build/"
    OLD_RPATH="`patchelf --print-rpath submodules/godot/bin/godot.x11.tools.64`"
    if [[ $OLD_RPATH != $PATH_TO_SIMULA_WLROOTS* ]]; then # Check if the current RPATH contains our local simula wlroots build. If not, patchelf it to add it
        echo "Patching godot.x11.tools to point to local wlroots lib"
        echo "Changing path to: $PATH_TO_SIMULA_WLROOTS:$OLD_RPATH"
        patchelf --set-rpath "$PATH_TO_SIMULA_WLROOTS:$OLD_RPATH" submodules/godot/bin/godot.x11.tools.64
    else
        echo "Not patching godot.x11.tools, already patched."
    fi
}

# devBuild = true function
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
