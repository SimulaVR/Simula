Building with Nix
=================

The easiest way to build Simula is to install `nix` and run::

    stack --nix build --ghc-options="-pgmcg++ -pgmlg++"
    source ./swrast.sh # only needs to be run once

Nix automatically downloads every non-Haskell dependency for this project and places them in */nix/store* in such a way that they don't conflict with your current distro's libraries. Running *stack* with these flags tells it how to find these libraries. The *swrast.sh* script tells nix how to find your system's OpenGL drivers.

If you don't already have *nix* installed, you can get it from your distro's package manager, or run

``curl https://nixos.org/nix/install | sh``

To use *simulavr* see :ref:`simulavr-usage`.
