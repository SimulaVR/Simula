{ stdenv, fetchFromGitHub, haskellPackages, callPackage, buildEnv, xrdb, wmctrl, SDL2, lib, onNixOS ? false, xwayland, xkbcomp, ghc, ffmpeg-full, midori, xfce, devBuild, fontconfig, glibcLocales, dejavu_fonts, writeScriptBin, coreutils, curl, vulkan-loader, mimic, xsel, xclip, dialog, synapse, openxr-loader, xpra, valgrind, xorg, writeShellScriptBin, python3, awscli, wayland, wayland-protocols, zstd, profileBuild ? false, pkgs, patchelf, libv4l, openssl, cabal-install}:
let

    /* Modify a stdenv so that it produces debug builds; that is,
      binaries have debug info, and compiler optimisations are
      disabled. */
    keepDebugInfo = stdenv: stdenv //
      { mkDerivation = args: stdenv.mkDerivation (args // {
          dontStrip = true;
          NIX_CFLAGS_COMPILE = toString (args.NIX_CFLAGS_COMPILE or "") + " -g -ggdb -Og";
        });
      };
    stdenvRes = if devBuild then (keepDebugInfo stdenv) else stdenv;
    xwayland-dev = xwayland.overrideAttrs (oldAttrs: {
      stdenv = stdenvRes;
    });
    libxcb-dev = xorg.libxcb.overrideAttrs (oldAttrs: {
      stdenv = stdenvRes;
    });
    wayland-dev = wayland.overrideAttrs (oldAttrs: {
      stdenv = stdenvRes;
    });
    wayland-protocols-dev = wayland-protocols.overrideAttrs (oldAttrs: {
      stdenv = stdenvRes;
    });
    wlroots-dev = callPackage ./submodules/wlroots/wlroots.nix { stdenv = stdenvRes; };
    vulkan-loader-custom = if onNixOS then vulkan-loader else (callPackage ./nix/vulkan-loader.nix { });
    glibc-locales = glibcLocales;
    godot = callPackage ./submodules/godot/godot.nix { devBuild = devBuild; onNixOS = onNixOS; };
    godot-api = "${godot}/bin/api.json";

    haskellCallPkg = if profileBuild then (pkgs.haskellPackagesPIC.callPackage) else (haskellPackages.callPackage);
    haskellCallPkgNoProfile = haskellPackages.callPackage;
    godot-haskell-classgen = haskellCallPkgNoProfile ./submodules/godot-haskell-cabal/classgen/classgen.nix { };
    godot-haskell = haskellCallPkg ./submodules/godot-haskell/godot-haskell.nix { api-json = godot-api; profileBuild = profileBuild; godot-haskell-classgen = godot-haskell-classgen; };
    godot-haskell-plugin = haskellCallPkg ./addons/godot-haskell-plugin/godot-haskell-plugin.nix { devBuild = devBuild; onNixOS = onNixOS; godot = godot; godot-haskell = godot-haskell; profileBuild = profileBuild; };

    monado = callPackage ./submodules/monado/monado.nix { gst-plugins-base = pkgs.gst_all_1.gst-plugins-base; gstreamer = pkgs.gst_all_1.gstreamer; };

    ghc-version = ghc.version;

    i3status = callPackage ./submodules/i3status {};

    devBuildFalse = ''
      ln -s ${godot}/bin/godot.x11.opt.debug.64 $out/bin/godot.x11.opt.debug.64
      ln -s ${godot}/bin/godot.x11.tools.64 $out/bin/godot.x11.tools.64
      ln -s ${godot}/bin/godot.x11.opt.64 $out/bin/godot.x11.opt.64

      cat << EOF > $out/bin/simula
      #!${stdenv.shell}
      ${xdgSetup}
      ${copyConfigFiles}

      echo "SIMULA_DATA_DIR: \$SIMULA_DATA_DIR"
      echo "SIMULA_CONFIG_DIR: \$SIMULA_CONFIG_DIR"
      echo "SIMULA_CACHE_DIR: \$SIMULA_CACHE_DIR"
      echo "SIMULA_LOG_DIR: \$SIMULA_LOG_DIR"
      echo "SIMULA_APP_DIR: \$SIMULA_APP_DIR"

      source $out/utils/Helpers.sh
      updateEmail
      (
        cd $out || exit 1
        source $out/utils/Helpers.sh
        PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib:${libv4l}/lib:$out/submodules/godot/modules/gdleapmotionV2/LeapSDK/lib/UnityAssets/Plugins/x86_64 \$($out/utils/GetNixGL.sh) ${godot}/bin/godot.x11.opt.debug.64 -m 2>&1 | ${coreutils}/bin/tee "\$SIMULA_DATA_DIR/log/output.file"
      )

      sed -i "s/\$USER/anon/g" "\$SIMULA_DATA_DIR/log/output.file"
      echo "devBuildFalse" >> "\$SIMULA_DATA_DIR/log/output.file"
      git rev-parse HEAD >> "\$SIMULA_DATA_DIR/log/output.file"
      cat "\$SIMULA_CONFIG_DIR/email" >> "\$SIMULA_DATA_DIR/log/output.file"
      ${curl}/bin/curl --data-urlencode errorMsg@\$SIMULA_DATA_DIR/log/output.file https://www.wolframcloud.com/obj/george.w.singer/errorMessage
      EOF

      chmod +x $out/bin/simula

      # simula-monado-service
      cat << EOF > $out/bin/simula-monado-service
      #!${stdenv.shell}
      ${xdgSetup}
      pkill monado-service
      ${monadoSetup}
      ${monado}/bin/monado-service 2>&1 | tee \$SIMULA_DATA_DIR/log/monado.log
      EOF
      chmod +x $out/bin/simula-monado-service
    '';

    copyConfigFiles = ''
      # Copy over default config files if they don't already exist
      if [ ! -f "\$SIMULA_CONFIG_DIR/HUD.config" ]; then
        cp $out/config/HUD.config "\$SIMULA_CONFIG_DIR/HUD.config"
      fi

      if [ ! -f "\$SIMULA_CONFIG_DIR/config.dhall" ]; then
        cp $out/config/config.dhall "\$SIMULA_CONFIG_DIR/config.dhall"
      fi

      if [ ! -d "\$SIMULA_DATA_DIR/environments" ]; then
        cp -R $out/environments "\$SIMULA_DATA_DIR/environments"
      fi
    '';

    xdgSetup = ''
      export XDG_DATA_HOME=''${XDG_DATA_HOME:-\$HOME/.local/share}
      export XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-\$HOME/.config}
      export XDG_CACHE_HOME=''${XDG_CACHE_HOME:-\$HOME/.cache}

      export SIMULA_DATA_DIR="\$XDG_DATA_HOME/Simula"
      export SIMULA_CONFIG_DIR="\$XDG_CONFIG_HOME/Simula"
      export SIMULA_CACHE_DIR="\$XDG_CACHE_HOME/Simula"

      mkdir -p "\$SIMULA_DATA_DIR/log"
      mkdir -p "\$SIMULA_CONFIG_DIR"
      mkdir -p "\$SIMULA_CACHE_DIR"
      export SIMULA_LOG_DIR="\$SIMULA_DATA_DIR/log"
      export SIMULA_APP_DIR="$out/bin"
    '';

    monadoSetup = ''
      export SIMULA_CONFIG_PATH=./config/simula_monado_config.json
      export XR_RUNTIME_JSON=./config/active_runtime.json
      export XRT_COMPOSITOR_LOG=debug
      export XRT_COMPOSITOR_SCALE_PERCENTAGE=100
    '';


    devBuildTrue = ''
      cp ./utils/GetNixGL.sh $out/bin/GetNixGL.sh

      # monado_local
      cat << EOF > $out/bin/monado_local
      #!${stdenv.shell}
      pkill monado-service
      ${monadoSetup}
      ./submodules/monado/build/src/xrt/targets/service/monado-service 2>&1 | tee .monado.log
      EOF
      chmod +x $out/bin/monado_local

      # monado_local
      echo "pkill monado-service" >> $out/bin/monado_local
      echo "export SIMULA_CONFIG_PATH=./config/simula_monado_config.json" >> $out/bin/monado_local
      echo "export XR_RUNTIME_JSON=./config/active_runtime.json" >> $out/bin/monado_local
      echo "export XRT_COMPOSITOR_LOG=debug" >> $out/bin/monado_local
      echo "export XRT_COMPOSITOR_SCALE_PERCENTAGE=100" >> $out/bin/monado_local
      echo "./submodules/monado/build/src/xrt/targets/service/monado-service 2>&1 | tee .monado.log" >> $out/bin/monado_local
      chmod +x $out/bin/monado_local

      # simula_local
      cat << EOF > $out/bin/simula_local
      #!${stdenv.shell}
      ${xdgSetup}
      ${copyConfigFiles}
      ${monadoSetup}
      export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive
      PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib:${libv4l}/lib:$out/submodules/godot/modules/gdleapmotionV2/LeapSDK/lib/UnityAssets/Plugins/x86_64 \$(./utils/GetNixGL.sh) ./submodules/godot/bin/godot.x11.tools.64 -m
      EOF
      chmod +x $out/bin/simula_local

      # simula_local_editor
      cat << EOF > $out/bin/simula_local_editor
      #!${stdenv.shell}
      ${xdgSetup}
      ${copyConfigFiles}
      ${monadoSetup}
      export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive
      mkdir -p log
      mkdir -p config
      PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib:${libv4l}/lib:./submodules/godot/modules/gdleapmotionV2/LeapSDK/lib/UnityAssets/Plugins/x86_64 \$(./utils/GetNixGL.sh) ./submodules/godot/bin/godot.x11.tools.64 -e "\$@"
      EOF
      chmod +x $out/bin/simula_local_editor

      # simula_local_profile
      cat << EOF > $out/bin/simula_local_profile
      #!${stdenv.shell}
      ${xdgSetup}
      ${copyConfigFiles}
      ${monadoSetup}
      export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive
      mkdir -p log
      mkdir -p config
      GHCRTS='-hc -p' PROFILE=1 PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib:./submodules/godot/modules/gdleapmotionV2/LeapSDK/lib/UnityAssets/Plugins/x86_64 \$(./utils/GetNixGL.sh) ./submodules/godot/bin/godot.x11.tools.64 -m
      EOF
      chmod +x $out/bin/simula_local_profile

      # simula_gdb
      cat << EOF > $out/bin/simula_gdb
      #!${stdenv.shell}
      ${xdgSetup}
      ${copyConfigFiles}
      ${monadoSetup}
      PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${libv4l}/lib:./submodules/godot/modules/gdleapmotionV2/LeapSDK/lib/UnityAssets/Plugins/x86_64 \$(./utils/GetNixGL.sh) gdb -x ./.gdbinit ./submodules/godot/bin/godot.x11.tools.64
      cat gdb.txt
      EOF
      chmod +x $out/bin/simula_gdb

      # simula_apitrace
      cat << EOF > $out/bin/simula_apitrace
      #!${stdenv.shell}
      ${xdgSetup}
      ${copyConfigFiles}
      ${monadoSetup}
      rm *.trace
      PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:./submodules/godot/modules/gdleapmotionV2/LeapSDK/lib/UnityAssets/Plugins/x86_64 \$(./utils/GetNixGL.sh) apitrace trace --api gl ./submodules/bin/godot.x11.tools.64
      apitrace dump *.trace | grep glTex > glTex.trace
      EOF
      chmod +x $out/bin/simula_apitrace

      # simula_valgrind
      ln -s ${valgrind}/bin/valgrind $out/bin/valgrind
      cat << EOF > $out/bin/simula_valgrind
      #!${stdenv.shell}
      ${xdgSetup}
      ${copyConfigFiles}
      ${monadoSetup}
      export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive
      PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib \$(./utils/GetNixGL.sh) ./result/bin/valgrind --tool=memcheck --leak-check=yes --show-reachable=yes --track-origins=yes --keep-stacktraces=alloc-and-free --error-limit=no --num-callers=40 --xml=yes --xml-file=valgrind_output_%p.xml ./submodules/godot/bin/godot.x11.tools.64 -m
      EOF
      chmod +x $out/bin/simula_valgrind

      # rootston
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/rootston
      echo "PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) ./submodules/wlroots/build/rootston/rootston \"\$@\"" >> $out/bin/rootston
      chmod +x $out/bin/rootston

      # demo_local
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/demo_local
      echo "mkdir -p log" >> $out/bin/demo_local
      echo "mkdir -p config" >> $out/bin/demo_local
      echo "PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib:${libv4l}/lib \$(./utils/GetNixGL.sh) ./submodules/godot/bin/godot.x11.tools.64 -m --path ./submodules/Demo" >> $out/bin/demo_local
      chmod +x $out/bin/demo_local

      # demo_local_editor
      echo "export LOCALE_ARCHIVE=${glibc-locales}/lib/locale/locale-archive" >> $out/bin/demo_local_editor
      echo "mkdir -p log" >> $out/bin/demo_local_editor
      echo "mkdir -p config" >> $out/bin/demo_local_editor
      echo "PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib:${openxr-loader}/lib:${libv4l}/lib \$(./utils/GetNixGL.sh) ./submodules/godot/bin/godot.x11.tools.64 -e --path ./submodules/Demo" >> $out/bin/demo_local_editor
      chmod +x $out/bin/demo_local_editor

      # demo_apitrace
      echo "rm *.trace" >> $out/bin/demo_apitrace
      echo "PATH=${xwayland-dev}/bin:${xkbcomp}/bin:\$PATH LD_LIBRARY_PATH=${SDL2}/lib:${vulkan-loader-custom}/lib \$(./utils/GetNixGL.sh) apitrace trace --api gl ./submodules/bin/godot.x11.tools.64 --path ./submodules/Demo" >> $out/bin/demo_apitrace
      echo "apitrace dump *.trace | grep glTex > glTex.trace" >> $out/bin/demo_apitrace
      chmod +x $out/bin/demo_apitrace


      mkdir -p $out/srcs/xwayland
      tar -xvf ${xwayland-dev.src} --directory $out/srcs/xwayland --strip-components=1
      mkdir -p $out/srcs/libxcb
      tar -xvf ${libxcb-dev.src} --directory $out/srcs/libxcb --strip-components=1
      mkdir -p $out/srcs/wayland
      tar -xvf ${wayland-dev.src} --directory $out/srcs/wayland --strip-components=1
      ln -s ${pernoscoSubmit}/bin/pernosco_submit $out/bin/pernosco_submit
      ln -s ${rrSources}/bin/rr_sources $out/bin/rr_sources

      ln -s ${cabal-install}/bin/cabal $out/bin/cabal
      ln -s ${ghc}/bin/ghc $out/bin/ghc
      '';

    devBuildScript = if (devBuild == true) then devBuildTrue else devBuildFalse;

    # Ensure xfce4-terminal has working fonts on every distribution
    xfce4-terminal-wrapped = writeScriptBin "xfce4-terminal" ''
      #!${stdenv.shell}
      export XDG_DATA_HOME=${dejavu_fonts}/share
      cd $HOME
      exec ${xfce.xfce4-terminal}/bin/xfce4-terminal "$@"
'';

    midori-wrapped = writeScriptBin "midori" ''
      #!${stdenv.shell}
      export XDG_DATA_HOME=${dejavu_fonts}/share
      exec ${midori}/bin/midori
      '';

    i3status-wrapped = writeScriptBin "i3status" ''
      #!${stdenv.shell}
      export LC_ALL=C
      exec ${i3status}/bin/i3status "$@"
      '';

    simulaPackages = if devBuild == true then [ valgrind ] else [ godot godot-haskell-plugin ];
    linkGHP = if devBuild == true then "" else ''
      ln -s ${godot-haskell-plugin}/lib/ghc-${ghc-version}/lib/libgodot-haskell-plugin.so $out/bin/libgodot-haskell-plugin.so;
      ln -s ${godot-haskell-plugin}/lib/ghc-${ghc-version}/lib/libgodot-haskell-plugin.so $out/addons/godot-haskell-plugin/libgodot-haskell-plugin.so
    '';
      # ln -s $out/bin/libgodot-haskell-plugin.so $out/addons/godot-haskell-plugin/libgodot-haskell-plugin.so;
      # ln -s ${godot-haskell-plugin}/lib/ghc-${ghc-version}/lib/libgodot-haskell-plugin.so $out/addons/godot-haskell-plugin/libgodot-haskell-plugin.so
      # rm $out/addons/godot-haskell-plugin/libgodot-haskell-plugin.so
      # ln -s ${godot-haskell-plugin}/lib/ghc-${ghc-version}/lib/libgodot-haskell-plugin.so $out/result/bin/libgodot-haskell-plugin.so;

    rrSources = writeShellScriptBin "rr_sources" ''
      RR_LOG=all:debug ./result/bin/rr sources \
      --substitute=libwlroots.so.0.0.0=$PWD/submodules/wlroots/backend \
      --substitute=libwayland-client.so.0.3.0=$PWD/result/srcs/wayland/src \
      --substitute=libwayland-egl.so.1.0.0=$PWD/result/srcs/wayland/egl \
      --substitute=libwayland-server.so.0.1.0=$PWD/result/srcs/wayland/src \
      --substitute=libxcb.so.1.1.0=$PWD/result/srcs/libxcb/src \
      --substitute=Xwayland=$PWD/result/srcs/xwayland/doc \
      ./rr/latest-trace \
      > sources.txt 2>&1
    '';

    pernoscoSubmit = writeShellScriptBin "pernosco_submit" ''
      PATH=${zstd}/bin:${awscli}/bin:./result/bin:$PATH ${python3}/bin/python3 ./submodules/pernosco-submit/pernosco-submit \
        -x \
      upload \
      --title $2 \
      --substitute=libwlroots.so.0.0.0=$PWD/submodules/wlroots/backend \
      --substitute=libwayland-client.so.0.3.0=$PWD/result/srcs/wayland/src \
      --substitute=libwayland-egl.so.1.0.0=$PWD/result/srcs/wayland/egl \
      --substitute=libwayland-server.so.0.1.0=$PWD/result/srcs/wayland/src \
      --substitute=libxcb.so.1.1.0=$PWD/result/srcs/libxcb/src \
      --substitute=Xwayland=$PWD/result/srcs/xwayland/doc \
      $1 ./. \
      $PWD/submodules/wlroots \
      $PWD/result/srcs \
      $PWD/result/srcs/wayland/src \
      $PWD/result/srcs/wayland/egl \
      $PWD/result/srcs/libxcb/src \
      $PWD/result/srcs/xwayland/doc \
      $PWD/submodules/Demo \
      ${wayland-dev} \
      ${wlroots-dev} \
      > pernosco.txt 2>&1
    '';

    simula = stdenv.mkDerivation {
      name = "Simula";
      src = ./.;
      buildInputs = [ xpra xrdb wmctrl fontconfig glibc-locales xfce4-terminal-wrapped openxr-loader midori-wrapped pernoscoSubmit i3status-wrapped cabal-install ghc ] ++ simulaPackages;
      installPhase = ''
      mkdir -p $out

      cp -R $src/* $out/ # Copy over Simula files so we can launch it from the nix store
      cp $src/.Xdefaults $out/ || echo "Failed to copy .Xdefaults" # Hack: for some reason this file has trouble copying unless we do it explicitly?
      chmod -R u+w $out # Allows us to create the libgodot-haskell-plugin.so symlink

      mkdir -p $out/bin
      mkdir -p $out/srcs
      mkdir -p $out/share

      ln -s ${xpra}/bin/xpra $out/bin/xpra
      ln -s ${xfce4-terminal-wrapped}/bin/xfce4-terminal $out/bin/xfce4-terminal
      ln -s ${xrdb}/bin/xrdb $out/bin/xrdb
      ln -s ${wmctrl}/bin/wmctrl $out/bin/wmctrl
      ln -s ${ffmpeg-full}/bin/ffplay $out/bin/ffplay
      ln -s ${ffmpeg-full}/bin/ffmpeg $out/bin/ffmpeg
      ln -s ${midori-wrapped}/bin/midori $out/bin/midori
      ln -s ${synapse}/bin/synapse $out/bin/synapse
      ln -s ${xsel}/bin/xsel $out/bin/xsel
      ln -s ${mimic}/bin/mimic $out/bin/mimic
      ln -s ${xclip}/bin/xclip $out/bin/xclip
      ln -s ${patchelf}/bin/patchelf $out/bin/patchelf
      ln -s ${dialog}/bin/dialog $out/bin/dialog
      ln -s ${curl}/bin/curl $out/bin/curl
      ln -s ${i3status-wrapped}/bin/i3status $out/bin/i3status

      '' + linkGHP + devBuildScript;
    };

in simula
