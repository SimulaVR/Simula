Building with Nix
=================

The easiest way to build Simula is to install `nix` and run::

    stack --nix build
    source ./swrast.sh # only needs to be run once

Nix automatically downloads every non-Haskell dependency for this project and places them in `/nix/store` in such a way that they don't conflict with your current distro's libraries. Running `stack` with these flags tells it how to find these libraries. The `swrast.sh` script tells nix how to find your system's OpenGL drivers.

If you don't already have `nix` installed, you can get it from your distro's package manager, or run

``curl https://nixos.org/nix/install | sh``

To launch the `simulavr`, run
     ``stack [--nix --no-exec-pure] exec simulavr # include the flags only if you built w/nix``

This should launch a white screen compositor. In order to see more, you have to launch a Wayland application, such as

``weston-terminal``

Vive-Compositor
---------------

You will need two terminals to launch this compositor. In the first terminal, you must launch the OSVR server::

    nix-shell ./shell.nix                    # not needed unless you built the project with nix
    osvr_server ./config/ViveDirectMode.json # or use ViveExtendedMode.json for extended mode

In the second terminal, launch the `simulavr`:

``stack [--nix --no-exec-pure] exec simulavr # include the flags only if you built w/nix``

**NOTE:** you will need `nvidia-381.26.13` (or higher) video drivers. Unfortunately, nix cannot provide those, so you will have to get them from your system's package manager.

Troubleshooting
===============

For assistance, [go here](https://gitter.im/SimulaVR/Simula).
