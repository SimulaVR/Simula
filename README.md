# SimulaHS

The process to try out `SimulaHS`, is as simple as 1, 2, 3, 4a or 4b, 5 with only minor software compliation in between. You will need to install software from your linux distributor and have your machine whirl and buzz for minutes on end with no output to test this collection of Haskell, C, and C++ code. If you are up for the challenge then you can find most of a recipie below. Many of the instructions have only been attempted once, if any at all, and can only be cast as a guide to get running. With those precautions aside, here's what someone got working once, sometime, probably.

Step 1: Clone all submodules
Step 2: Build and Install requisite packages for your system
Step 3: Build SimulaHS with
    stack build --extra-lib-dirs="${HOME}"/.local/lib --extra-include-dirs="${HOME}"/.local/include
Step 4a (_Default_): Run OSVR Server
    osvr_server

Step 4b (_Advanced_): Run OSVR Server with a configuation file
    osvr_server /path/to/configuration-file.json

Step 5: Run the simple compositor with
    stack exec simple-compositor

# Cloning Submodules

Before you begin you must initialize and update the submodules in the `SimulaHS` repository.  This process ensures that you are in sync with previously checked in combinations of commits between the two projects. More information on submodules is [here](https://git-scm.com/book/en/v2/Git-Tools-Submodules).

## Initialization

    git submodule init

## Update

    git submodule update

# Ubuntu 17.04 (zesty) Required packages
Install all the dependecies in one shot with the following script
```
sudo apt install \
    g++ \
    automake \
    autoconf \
    autoconf-archive \
    make \
    cmake \
    libtool \
    pkg-config \
    binutils-dev \
    libegl1-mesa-dev \
    libgles2-mesa-dev \
    libxcb-composite0-dev \
    libxcursor-dev \
    libcairo2-dev \
    libpixman-1-dev \
    libgbm-dev \
    libmtdev-dev \
    libinput-dev \
    libxkbcommon-dev \
    libpam0g-dev \
    libgflags-dev \
    libgoogle-glog-dev \
    libssl-dev \
    libdouble-conversion-dev \
    libevent-dev \
    libboost-context-dev \
    libboost-chrono-dev \
    libboost-filesystem-dev \
    libboost-iostreams-dev \
    libboost-locale-dev \
    libboost-program-options-dev \
    libboost-regex-dev \
    libboost-system-dev \
    libboost-thread-dev \
    libsdl2-dev \
    libopencv-dev \
    libjsoncpp-dev \
    libxml2-dev \
    libusb-1.0-0-dev \
    libspdlog-dev \
    libeigen3-dev

```

# Git Repos Needed -- In Order

1. wayland-protocols - [https://github.com/wayland-project/wayland-protocols]
  Currently HEAD is fine, but any tag >= 1.7 will work fine.

2. libweston - [https://github.com/wayland-project/weston]
  You will need to checkout tag `2.0.0`.

3. libfunctionality - [https://github.com/OSVR/libfunctionality]
  Currently HEAD is fine, previous versions not tested (2017-07-20).

4. folly - [https://github.com/facebook/folly]

5. OSVR-Core - [https://github.com/OSVR/OSVR-Core]
  Currently HEAD is fine, v0.6 does not build on debian stretch.

## System packages

### Dependencies for `wayland-protocols`

There are no special dependencies, but a pro-tip, run `configure` with a custom `PREFIX` and copy the `wayland-protocols.pc` file to `PREFIX/lib/pkgconfig` and set `PKG_CONFIG_PATH` to the same.

Example
    ./configure --prefix="$HOME"/.local
    make && make install
    cp wayland-protocols.pc "$HOME"/.local/lib/pkgconfig
    export PKG_CONFIG_PATH="$HOME"/.local/lib/pkgconfig

### Dependencies for `libweston`

Make sure you have installed `wayland-protocols` before proceeding to building `libweston`.

1. EGL - libegl1-mesa-dev
2. glesv2 - libgles2-mesa-dev
3. xcb-composite - libxcb-composite0-dev
4. xcursor - libxcursor-dev
5. cairo-xcb - libcairo2-dev
6. automatically install by libcairo2-dev - libpixman-1-dev
7. gbm - libgbm-dev
8. mtdev - libmtdev-dev
9. libinput - libinput-dev
10. xkbcommon - libxkbcommon-dev
11. pam - libpam0g-dev

After installing the above packages you can configure and build `libweston`. Here is a recipie for success:

    git checkout -b v2.0.0 2.0.0
    ./autogen.sh
    ./configure --prefix="$HOME"/.local --disable-setuid-install
    make && make install

You will see a notice about needing to set `LD_LIBRARY_PATH` and also for setting `LD_RUN_PATH` to use these newly installed libraries. You may want to set these in your `.bashrc` file or other shell startup file. For your interactive shell you can just use the following lines:

    LIBDIR="$HOME"/.local/lib
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH":"$LIBDIR":"$LIBDIR"/libweston-2:"$LIBDIR"/weston
    export LD_RUN_PATH="$LD_RUN_PATH:"$LIBDIR":"$LIBDIR"/libweston-2:"$LIBDIR"/weston

### Dependencies for `libfunctionality`

You will need `cmake` to build any of the projects from `OSVR`. When building `cmake` projects you should perform out-of-tree builds by creating a build directory and running `cmake` from that directory. For example you can repeat this pattern for any cmake project:

    mkdir $PROJECT-build
    git clone $PROJECT_URI
    cd $PROJECT-build
    cmake ../$PROJECT

To set a custom `PREFIX` for cmake projects you need to use the following incantation.

    cmake -D CMAKE_INSTALL_PREFIX="$HOME"/.local ../$PROJECT

### Dependencies for `folly`

1. boost-context - libboost-context-dev
2. boost-chrono - libboost-chrono-dev
3. boost-filesystem - libboost-filesystem-dev
4. boost-regex - libboost-regex-dev
5. boost-program-options - libboost-program-options-dev
6. boost-system - libboost-system-dev
7. boost-thread - libboost-thread-dev
8. gflags - libgflags-dev
9. google-glog - libgoogle-glog-dev
10. libssl - libssl-dev
11. double-conversion - libdouble-conversion-dev
12. libevent - libevent-dev

To build folly you need to run `autoreconf -ivf` from the folly subdirectory of the cloned repository.

    cd folly
    autoreconf -ivf
    ./configure --prefix="$HOME"/.local
    make && make install

### Dependencies for `OSVR-Core`

To proceed ensure you have installed `folly`, `libfunctionality`, `libweston`, and `wayland-protocols` as described above.

1. sdl2 - libsdl2-dev
2. opencv - libopencv-dev
3. jsoncpp - libjsoncpp-dev
4. boost-thread - libboost-thread-dev
5. boost-locale - libboost-locale-dev
6. boost-filesystem - libboost-filesystem-dev
7. boost-program-options - libboost-program-options-deu
8. libusb - libusb-1.0-0-dev
9. libspdlog - libspdlog-dev

When fetching from github you must fetch the submodules and initialize them before attempting a build.
    git submodule update --init --recursive

OSVR-Core is a `cmake` project so refer to the instructions above in the `libfunctionality` section to perform an out-of-tree build.

# Debian (stretch) Required packages

__WARNING__: _You cannot run the HMD OSVR code on Debian at this time
you can, however, build this repository without the updated kernel
drivers provided by Ubuntu._

Install all the dependecies in one shot with the following script
```
sudo apt install \
    g++ \
    automake \
    autoconf \
    autoconf-archive \
    make \
    cmake \
    libtool \
    pkg-config \
    binutils-dev \
    libegl1-mesa-dev \
    libgles2-mesa-dev \
    libxcb-composite0-dev \
    libxcursor-dev \
    libcairo2-dev \
    libpixman-1-dev \
    libgbm-dev \
    libmtdev-dev \
    libinput-dev \
    libxkbcommon-dev \
    libpam0g-dev \
    libgflags-dev \
    libgoogle-glog-dev \
    libssl-dev \
    libdouble-conversion-dev \
    libevent-dev \
    libboost-context-dev \
    libboost-chrono-dev \
    libboost-filesystem-dev \
    libboost-locale-dev \
    libboost-program-options-dev \
    libboost-regex-dev \
    libboost-system-dev \
    libboost-thread-dev \
    libsdl2-dev \
    libopencv-dev \
    libjsoncpp-dev \
    libusb-1.0-0-dev \
    libspdlog-dev

```

Detailed instructions on which git repositories are needed and special instructions for each one are given below. You will also need the Haskell `stack` tool available for download at [https://docs.haskellstack.org/en/stable/README/]. Once all the dependencies are built and installed you can run `stack setup` followed by `stack build` in the top level of SimulaHS to get a build.


