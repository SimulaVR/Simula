# The following functions assume they are called from project root.

export SIMULA_NIX=$(pwd)/nix-forced-version/bin

checkInstallNix() {
    if command -v nix; then
        echo "nix already installed.."
    else
        curl -L https://nixos.org/nix/install | sh
        . $HOME/.nix-profile/etc/profile.d/nix.sh
    fi
}

# Bootstrop the proper versions of nix-* commands for building Simula
buildNixForcedVersion() {
    nix-build nix-forced-version.nix -o nix-forced-version # this is the last time we system-local `nix`!
}

checkInstallCachix() {
    if command -v cachix; then
        echo "cachix already installed.."
    else
        $SIMULA_NIX/nix-env -iA cachix -f https://cachix.org/api/v1/install
    fi
}

checkIfNixOS() {
    if [ -e /etc/NIXOS ]; then
        echo "true";
    else
        echo "false";
    fi
}

switchToNix() {
    cd ./addons/godot-haskell-plugin
    rm -f libgodot-haskell-plugin.so
    ln -s ../../result/bin/libgodot-haskell-plugin.so libgodot-haskell-plugin.so
    cd -
}

switchToLocal() {
    cd ./addons/godot-haskell-plugin
    rm -f libgodot-haskell-plugin.so
    path=$($SIMULA_NIX/nix-shell -Q shell.nix --run "../../result/bin/cabal list-bin flib:godot-haskell-plugin")
    ln -s "$path" libgodot-haskell-plugin.so
    cd -
}

checkInstallCurl() {
    if command -v curl; then
        echo "curl already installed.."
    else
        $SIMULA_NIX/nix-env -iA nixpkgs.curl
    fi
}

installSimula() {
    checkInstallNix
    buildNixForcedVersion
    checkInstallCachix
    checkInstallCurl
    cachix use simula
    curl https://www.wolframcloud.com/obj/george.w.singer/installMessage
    if [ -z $1 ]; then
        NIXPKGS_ALLOW_UNFREE=1 $SIMULA_NIX/nix-build -Q default.nix --arg onNixOS "$(checkIfNixOS)" --arg devBuild "false"
        switchToNix
        # Useful for debug purposes
    elif [ "$1" = "i" ]; then
        switchToNix
        NIXPKGS_ALLOW_UNFREE=1 $SIMULA_NIX/nix-instantiate -Q -K default.nix --arg onNixOS "$(checkIfNixOS)" --arg devBuild "true"
        switchToLocal
    else
        switchToNix
        NIXPKGS_ALLOW_UNFREE=1 $SIMULA_NIX/nix-build -Q -K default.nix --arg onNixOS "$(checkIfNixOS)" --arg devBuild "true"
        switchToLocal
    fi
}


checkInstallGit() {
    if command -v git; then
        echo "git already installed.."
    else
        $SIMULA_NIX/nix-env -iA nixpkgs.git
    fi
}

updateSimula() {
    checkInstallNix
    checkInstallCachix
    checkInstallGit
    cachix use simula

    if [ -z $1 ]; then
        git pull origin master
        git submodule update --recursive
        NIXPKGS_ALLOW_UNFREE=1 $SIMULA_NIX/nix-build -Q default.nix --arg onNixOS "$(checkIfNixOS)" --arg devBuild "false"
        switchToNix
    else
        switchToNix
        git pull origin dev
        git submodule update --recursive
        NIXPKGS_ALLOW_UNFREE=1 $SIMULA_NIX/nix-build -Q -K default.nix --arg onNixOS "$(checkIfNixOS)" --arg devBuild "false"
        switchToNix
    fi
}

nsBuildMonado() {
  cd ./submodules/monado
  $SIMULA_NIX/nix-shell shell.nix --run nsBuildMonadoIncremental
  cd -
}

nsCleanMonado() {
  cd ./submodules/monado
  $SIMULA_NIX/nix-shell shell.nix --run rmBuilds
  cd -
}

# Experimental nsBuild* functions allow Simula developers to locally build
# Simula modules inside a nix-shell
nsBuildGodot() {
 cd ./submodules/godot
 local runCmd="wayland-scanner server-header ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.h; wayland-scanner private-code ./modules/gdwlroots/xdg-shell.xml ./modules/gdwlroots/xdg-shell-protocol.c; scons -Q -j8 platform=x11 target=debug warnings=no"; 

 if [ -z $1 ]; then
   $SIMULA_NIX/nix-shell --run "$runCmd"
 else
   $SIMULA_NIX/nix-shell --run "while inotifywait -qqre modify .; do $runCmd; done"
 fi
 cd -
}

nsCleanGodot() {
    cd ./submodules/godot
    local runCmd="scons --clean"
    $SIMULA_NIX/nix-shell --run "$runCmd"
    cd -
}

# Updates godot-haskell to latest api.json generated from devBuildGodot
nsBuildGodotHaskell() {
  cd ./submodules/godot
  $SIMULA_NIX/nix-shell -Q --run "LD_LIBRARY_PATH=./modules/gdleapmotionV2/LeapSDK/lib/x64 $(../../utils/GetNixGL.sh) ./bin/godot.x11.tools.64 --gdnative-generate-json-api ./bin/api.json"
  cd -

  cd ./submodules/godot-haskell-cabal
  if [ -z $1 ]; then
    $SIMULA_NIX/nix-shell -Q release.nix --run "./updateApiJSON.sh"
  elif [ $1 == "--profile" ]; then
    $SIMULA_NIX/nix-shell -Q --arg profileBuild true release.nix --run "./updateApiJSON.sh"
  fi
  cd -
}

nsBuildGodotHaskellPlugin() {
  cd ./addons/godot-haskell-plugin
  if [ -z $1 ]; then
    $SIMULA_NIX/nix-shell -Q shell.nix --run "../../result/bin/cabal build"
  elif [ $1 == "--profile" ]; then
    $SIMULA_NIX/nix-shell -Q shell.nix --arg profileBuild true --run "../../result/bin/cabal --enable-profiling build --ghc-options=\"-fprof-auto -rtsopts -fPIC -fexternal-dynamic-refs\""
  else
    $SIMULA_NIX/nix-shell shell.nix --run "while inotifywait -qqre modify .; do ../../result/bin/cabal build; done"
  fi
  cd -
}

nsREPLGodotHaskellPlugin() {
    cd ./addons/godot-haskell-plugin
    $SIMULA_NIX/nix-shell shell.nix --run "cabal repl"
}

# Takes optional argument for a profile build
nsBuildSimulaLocal() {
    installSimula 1                      || { echo "installSimula 1 failed"; return 1; } # forces dev build
    PATH=./result/bin:$PATH cabal update || { echo "cabal update failed"; return 1; }
    nsBuildMonado                        || { echo "nsBuildMonado failed"; return 1; }
    nsBuildWlroots                       || { echo "nsBuildWlroots failed"; return 1; }
    nsBuildGodot                         || { echo "nsBuildGodot failed"; return 1; }
    patchGodotWlroots                    || { echo "patchGodotWlroots failed"; return 1; }
    nsBuildGodotHaskell "$1"             || { echo "nsBuildGodotHaskell failed"; return 1; }
    nsBuildGodotHaskellPlugin "$1"       || { echo "nsBuildGodotHaskellPlugin failed"; return 1; }
    switchToLocal                        || { echo "switchToLocal failed"; return 1; }
}

nsBuildWlroots() {
    cd ./submodules/wlroots
    if [ -d "./build" ]; then
        $SIMULA_NIX/nix-shell -Q --run "ninja -C build"
    else
        $SIMULA_NIX/nix-shell -Q --run "meson build; ninja -C build"
    fi
    cd -
}

updateEmail() {
    if [ -e ./email ]; then
        # .. do nothing ..
        echo ""
    else
        ./result/bin/dialog --title "SimulaVR" --backtitle "OPTIONAL: Provide email for important Simula updates & improved bug troubleshooting" --inputbox "Email: " 8 60 --output-fd 1 > ./email 2>&1
        ./result/bin/curl --data-urlencode emailStr@email https://www.wolframcloud.com/obj/george.w.singer/emailMessage
        clear
    fi
}

#patch our Godot executable to point to our local build of wlroots
patchGodotWlroots(){
    PATH_TO_SIMULA_WLROOTS="`pwd`/submodules/wlroots/build/"
    OLD_RPATH="`./result/bin/patchelf --print-rpath submodules/godot/bin/godot.x11.tools.64`"
    if [[ $OLD_RPATH != $PATH_TO_SIMULA_WLROOTS* ]]; then #check if the current rpath contains our local simula wlroots build. If not, patchelf to add our path to the start of the executable's rpath
        echo "Patching godot.x11.tools to point to local wlroots lib"
        echo "Changing path to: $PATH_TO_SIMULA_WLROOTS:$OLD_RPATH"
        ./result/bin/patchelf --set-rpath "$PATH_TO_SIMULA_WLROOTS:$OLD_RPATH" submodules/godot/bin/godot.x11.tools.64
    else
        echo "Not patching godot.x11.tools, already patched."
    fi
}

zenRR() {
   $SIMULA_NIX/nix-shell --arg onNixOS $(checkIfNixOS) --arg devBuild true --run "sudo python3 ./utils/zen_workaround.py"
}