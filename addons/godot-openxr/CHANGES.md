Changes to the Godot OpenXR asset
=================================

1.1.0
-------------------
- Implemented Android build (currently using Oculus loader, Quest support only)
- Fix invalid transforms generated from invalid space locations when using OpenXRSkeleton or OpenXRPose.

1.0.3
-------------------
- Copy loader dll in place when compiling
- Added mesh based hand scenes using Valve OpenXR hand meshes
- Updated to OpenXR 1.0.18
- Added action and interaction profile for thumbstick/joystick click, using button index 14 `JOY_VR_PAD`.

1.0.2
-------------------
- Fix folder structure of godot_openxr.zip created by Github actions

1.0.1
-------------------
- Fix crash issue on Oculus Link when taking headset off and putting it back on
- Add support for finger tracking motion range

1.0.0
-------------------
- Original implementation
- Switched to use godot-cpp
- Added actions and default profiles
