.. _simulavr-usage:

SimlaVR Usage
=============

To use *simulavr* you need to start an *osvr_server* to read the controller data. See the section, :ref:`osvr-server`, below for more details on using and configuring the *osvr_server*.

*SimulaVR* can be tested with and without additional hardware. Current limitations include the following:

    * Headset rendering is non-existent at the moment. Maybe you can help, see :ref:`rendering-roadmap-hmd` for more ways to help.
    * To get feedback on the headset movement when all hardware is connected and seen by *osvr_server* you must pass the *-w* or *--waitHMD* flag to *simulavr* to properly wait for the connection to *osvr_server*.

.. note::

  When everything is working properly you will see a boring white window. You should launch some programs for testing. The developers' goto test program is ``weston-terminal``.

With Nix
--------

You will need two terminals to launch this compositor. In the first terminal, you must launch the :ref:`osvr-server`.

``nix-shell ./shell.nix``

In the second terminal, launch *simulavr*:

``stack --nix --no-exec-pure exec -- simulavr [-h -w]``

.. NOTE::

    You will need *nvidia-381.26.13* (or higher) video drivers. Unfortunately, nix cannot provide those, so you will have to get them from your system's package manager.

Simply Stack
------------

Vanilla baseline testing and executing by only using *stack* along with system libraries.

``stack exec simulavr``

.. note::

  When run with no arguments *simulavr* will not track the headset. It will update controller positions, but not the headset.

If you have everything working and want to sync with the headset as well as the controllers use the following snippet.

``stack exec -- simulavr -w``

.. _osvr-server:

OSVR Server
-----------

``osvr_server ./config/ViveExtendedMode.json # or use ViveDirectMode.json for direct mode``
