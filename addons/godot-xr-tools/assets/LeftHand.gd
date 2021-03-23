extends Spatial

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	var controller : ARVRController = get_parent()
	if controller:
		var grip = controller.get_joystick_axis(JOY_VR_ANALOG_GRIP) * 2.5
		var trigger = controller.get_joystick_axis(JOY_VR_ANALOG_TRIGGER) * 2.5
		
		# print("Grip: " + str(grip) + " Trigger: " + str(trigger))
		
		$AnimationTree.set("parameters/SetGrip/seek_position", grip)
		$AnimationTree.set("parameters/SetIndex/seek_position", trigger)
		
		# var grip_state = controller.is_button_pressed(JOY_VR_GRIP)
		# print("Pressed: " + str(grip_state))
