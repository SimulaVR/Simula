Rendering Roadmap
=================

With the HTC-Vive using osvr, **simulavr** can see and get reports on the location information for the two controllers and the headset. The main renderer is a wayland server and provides a full compositor. OSVR is also providing all the distortion information needed to render static images to the headset display.

.. _rendering-roadmap-hmd:

Headset Rendering Status
------------------------

.. table:: Missing Puzzle Pieces in Rendering

    ============================ ===================== =========
    Feature to Implement         Implementation Status Priority
    ============================ ===================== =========
    Respond to Controllers       Partially implemented Blocker
    Respond to Buttons           Possible to implement Low
    Render Controllers           Needs Haskell         Normal
    Viewport Responds to HMD     Needs Haskell         Blocker
    Render Left Eye to Texture   Needs Haskell         Low
    Render Right Eye to Texture  Needs Haskell         Low
    Render to HMD Screen         library missing       High
    ============================ ===================== =========

HMD Rendering Notes
^^^^^^^^^^^^^^^^^^^

There are several libraries available for use in different aspects of VR. The two main libraries in use by Simula are `OSVR <osvr-link/>`_ and `OpenVR <openvr-link>`_. Both of these libraries are meant to be high level and provide a unified interface for dealing with inputs and headset rendering. `OpenVR <openvr-link>` depends on the `SteamVR <steamvr-link>`_ runtime library to work, but seems to be the most robust library for interfacing with the headset.

The `OSVR <osvr-link/>`_ library does a wonderful job of working with the HTC lighthouse devices and gives a callback registration interface for handling input devices. This works really well. It has the potential to scale to multiple headsets connected over a network. With all that said the headset portion of the library is still binary only and Linux support is non-existent at the moment. We have tried using `OSVR-RenderManager <osvr-renderman>`_ and can't seem to get direct or extended mode to render to the headset.

The current plan is to move ahead trying to work with `OpenVR <openvr-link>`_ and explore additional library options. `OpenHMD <openhmd-link>`_ is a potential library to integrate and test. There is the possibility of going more low-level and using another OpenGL context to just render textures. Another would be to simply get a dumb XOrg window and render the eye distortion textures there.

.. _osvr-link: http://osvr.github.io/

.. _steamvr-link: https://steamcommunity.com/steamvr

.. _openvr-link: https://github.com/ValveSoftware/openvr

.. _osvr-renderman: https://github.com/sensics/OSVR-RenderManager

.. _openhmd-link: https://github.com/OpenHMD/OpenHMD/

Evaluating Libraries
^^^^^^^^^^^^^^^^^^^^

When evaluating a library for inclusion, there are a couple of criteria to keep in mind.

#. Does it work?

#. Can you write a small example Haskell program using it?
