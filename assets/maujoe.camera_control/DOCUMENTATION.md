## Docummentation:


### Settings available via Editor/GDscript:

- bool enable : enable/disable camera controls. Default is true.
- int mouse_mode: Same as Godot's mouse settings by default the mouse is captured:
  - Visible = 0 (MOUSE_MODE_VISIBLE),
  - Hidden = 1 (MOUSE_MODE_HIDDEN),
  - Capture = 2 (MOUSE_MODE_CAPTURED),
  - Confined = 3 (MOUSE_MODE_CONFINED).


- bool mouselook - Enable/disable mouselook. Default is true.
- float sensitivity - Sensitivity of the mouselook. A value between 0 and 1. Default value is 0.5.
- float smoothness - Smoothness of the mouselook. A value between 0,001 and 0,999. Default value is 0.5.
- Spatial privot - Optional privot object for thirdperson like mouselook. Default value is None (no privot).
- bool rotate_privot - Enable/disable if the will be rotated with the camera. Default is false.
- float distance - The distance between the camera and the privot object. Minimum value is 0. Default value is 5.0
- bool rotate_privote - Rotate privot object with the mouselook. Default is false.
- bool collision - The camera avoid it to go through/behind objects. Default is true.
- int yaw_limit - Limit the yaw of the mouselook in Degrees, if limit >= 360 there is no limit. Default value is 360.
- int pitch_limit - Limit the Pitch of the mouselook in Degrees, if limit = 360 there is no limit. Default value is 360.


- bool movement - Enable/disable camera movement (flying). Default is true.
- bool local - Switch between movement on local or global axes. Default is true.
- float acceleration - Set the movement speed up factor. A Value between 0 and 1. Default value is 1.0.
- float deceleration - Set the movement slow down factor. A Value between 0 and 1. Default value is 0.1.
- Vector3 max_speed - Set maximum movement speed for each axes separately. Default value is (1.0, 1.0, 1.0).


- String forword_action - Input Action for fordward movement. Default action is "ui_up".
- String backward_action - Input Action for backward movement. Default action is "ui_down".
- String left_action - Input Action for Left movement. Default action is "ui_left".
- String right_action: Input Action for Right movement. Default action is "ui_right".
- String up_action - Input Action for upward movement. Default action is "ui_page_up".
- String down_action: Input Action for downward movement. Default action is "ui_page_down".


- String gui_action - Input Action to show/hide the ingame control gui. Default action is "ui_cancel".
- bool use_gui - Enable/disable ingame gui. Default is true.


### Gui configuration:

The ingame gui can also be configurated via constants in the camera_control_gui.gd script

- const Vector2 GUI_POS - The default position of the gui. Default is (10, 10).
- const Vector2 GUI_SIZE - The size of the gui. Default is (200, 0)
- const bool DRAGGABLE - Enable/disable draggable gui. Default is true.
- const bool CUSTOM_BACKGROUND - Enable/disable custom background color. Default is false.
- const Color CUSTOM_COLOR - Set custom background color.
- const MAX_SPEED - The maximal value of the speedslider
