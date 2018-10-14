setInstallState() {
  export SIMULA_WAYLAND_ROOT=$(cd ./addons/godot-haskell-plugin/simula-wayland && pwd)
  export INSTALL_DIR=$SIMULA_WAYLAND_ROOT/install

  mkdir -p $INSTALL_DIR

  export WLD=$INSTALL_DIR
  export LD_LIBRARY_PATH=$WLD/lib
  export PKG_CONFIG_PATH=$WLD/lib/pkgconfig/:$WLD/share/pkgconfig/
  export PATH=$WLD/bin:$PATH
  export ACLOCAL_PATH=$WLD/share/aclocal
  export ACLOCAL="aclocal -I $ACLOCAL_PATH"

  mkdir -p $WLD/share/aclocal # needed by autotools
}

installWayland() {
  cd $INSTALL_DIR
  # wayland
  git clone git://anongit.freedesktop.org/wayland/wayland-protocols
  cd wayland-protocols
  ./autogen.sh --prefix=$WLD
  make install
  cd ..
}

installWaylandProtocols() {
  cd $INSTALL_DIR
  git clone git://anongit.freedesktop.org/wayland/wayland-protocols
  cd wayland-protocols
  ./autogen.sh --prefix=$WLD
  make install
  cd ..
}

installMesa() {
  cd $INSTALL_DIR
  git clone git://anongit.freedesktop.org/mesa/mesa
  cd mesa
  ./autogen.sh --prefix=$WLD --enable-gles2 \
  --with-platforms=x11,wayland,drm --enable-gbm --enable-shared-glapi \
  --with-gallium-drivers=r300,r600,swrast,nouveau --enable-llvm
  make && make install
  cd ..
}

installLibUnwind() {
  cd $INSTALL_DIR
  git clone git://git.sv.gnu.org/libunwind
  cd libunwind
  autoreconf -i
  ./configure --prefix=$WLD
  make && make install
  cd ..
}

installLibInput() {
  echo "Omitting installation of libinput"
}

installWeston() {
  setInstallState
  local CURRENT_DIR=$(pwd)
  cd $INSTALL_DIR

  # Omitted due to `installMesa` being problematic on Ubuntu bionic
  # Simula currently only provides weston3 (and not any of its dependencies)
    # installWayland
    # installWaylandProtocols
    # installMesa
    # installLibUnwind
    # installLibInput

  cd $INSTALL_DIR
  git clone git://anongit.freedesktop.org/wayland/weston
  cd weston
  git checkout tags/3.0.0
  ./autogen.sh --prefix=$WLD
  make
  sudo make install
  cd ..

  cd $CURRENT_DIR
}