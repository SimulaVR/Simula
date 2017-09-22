# Simula's Horizon

| Goal                          | Status           | Short-Run Horizon | Long-Run Horizon |
|:------------------------------|:-----------------|:-----------------:|:----------------:|
| Basic, Launchable Compositor  | DONE             |         X         |                  | 
| Wayland App Compatibility     | DONE             |         X         |                  |
| X Applications Compatibility  |                  |         X         |                  | 
| HTC Vive Compatibility        |                  |         X         |                  | 
| Usable VR Desktop             |                  |         X         |                  | 
| Test Suite                    |                  |         X         |                  | 
| Clear Text Resolution in VR   |                  |                   |        X         | 
| Special-Purpose 3D Linux Apps |                  |                   |        X         | 
| A "VR Linux Distro"           |                  |                   |        X         | 
| Standalone HMD Compatibility  |                  |                   |        X         | 

# TODO Bucket

This is a compilation of old TODO notes. Important items from this list should moved to [Issues](https://github.com/SimulaVR/Simula/issues).

## General
- Awful C++-esque typeclass structure. In most cases, `Some a` can be replaced with something better. However due to time/etc limitations, the current code architecture closely mirrors the simula_cpp one.
- hs-boot files due to the above. Annoying extra bookkeeping and should be phased out whenever possible.
- General stinginess with comments. In case something is unclear, chances are it's either unclear in the motorcar source and/or unclear to me (since it was ported from motorcar/simula_cpp). Ask anyways though.
- MVars are used whenever mutable state is required. This is not always needed, and I've attempted to reduce the amount of mutable references, but it's likely I missed some.

## Simula.BaseCompositor
- .Event is basically unused. Weston handles all of that stuff.
- Geometry is mostly clean, apart from Rectangle being a misnomer (Rect is the actual Rectangle)
- OpenGL contains viewport code (relatively straightforward) and shader code (slightly suboptimal, as it compiled shaders too often; they should be cached)
- checkForErrors terminates the program on an OpenGL error with a backtrace; this can be deactivated by commentiong out the error line
- The scene graph is a big unholy mess of C++-esque code. It needs a massive refactoring.
- Gratitious use of RecursiveDo (aka mfix) to deal with <x needs reference of y, and vice versa, to be constructed> and superclass stuff. Could be solved by refactoring the above C++ mess.
- Generally, resources need to be cleaned up better. Consider ResourceT.
- The Weston surfaces currently don't have a type. This should be fixed.
- Weston.hs in general is a bit messy, and could use some cleaning up.
- The OSVR branch Weston.hs has modifications in the render loop and a reference to OSVR.hs' SimulaOSVRCLient. It should be merged back into the main branch.
