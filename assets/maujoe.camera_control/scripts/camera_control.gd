# Licensed under the MIT License.
# Copyright (c) 2018 Jaccomo Lorenz (Maujoe)

extends Camera

# User settings:
# General settings
export var enabled = true setget set_enabled
export(int, "Visible", "Hidden", "Caputered, Confined") var mouse_mode = 2

# Mouslook settings
export var mouselook = true
export (float, 0.0, 1.0) var sensitivity = 0.5
export (float, 0.0, 0.999, 0.001) var smoothness = 0.5 setget set_smoothness
export(NodePath) var privot setget set_privot
export var distance = 5.0 setget set_distance
export var rotate_privot = false
export var collisions = true setget set_collisions
export (int, 0, 360) var yaw_limit = 360
export (int, 0, 360) var pitch_limit = 360

# Movement settings
export var movement = true
export (float, 0.0, 1.0) var acceleration = 1.0
export (float, 0.0, 0.0, 1.0) var deceleration = 0.1
export var max_speed = Vector3(1.0, 1.0, 1.0)
export var local = true
export var forward_action = "ui_up"
export var backward_action = "ui_down"
export var left_action = "ui_left"
export var right_action = "ui_right"
export var up_action = "ui_page_up"
export var down_action = "ui_page_down"

# Gui settings
export var use_gui = true
export var gui_action = "ui_cancel"

# Intern variables.
var _mouse_position = Vector2(0.0, 0.0)
var _yaw = 0.0
var _pitch = 0.0
var _total_yaw = 0.0
var _total_pitch = 0.0

var _direction = Vector3(0.0, 0.0, 0.0)
var _speed = Vector3(0.0, 0.0, 0.0)
var _gui

func _ready():
	_check_actions([forward_action, backward_action, left_action, right_action, gui_action, up_action, down_action])

	if privot:
		privot = get_node(privot)
	else:
		privot = null

	set_enabled(enabled)

	if use_gui:
		_gui = preload("camera_control_gui.gd")
		_gui = _gui.new(self, gui_action)
		add_child(_gui)

func _input(event):
	if mouselook:
		if event is InputEventMouseMotion:
			_mouse_position = event.relative

	if movement:
		if event.is_action_pressed(forward_action):
			_direction.z = -1
		elif event.is_action_pressed(backward_action):
			_direction.z = 1
		elif not Input.is_action_pressed(forward_action) and not Input.is_action_pressed(backward_action):
			_direction.z = 0

		if event.is_action_pressed(left_action):
			_direction.x = -1
		elif event.is_action_pressed(right_action):
			_direction.x = 1
		elif not Input.is_action_pressed(left_action) and not Input.is_action_pressed(right_action):
			_direction.x = 0
			
		if event.is_action_pressed(up_action):
			_direction.y = 1
		if event.is_action_pressed(down_action):
			_direction.y = -1
		elif not Input.is_action_pressed(up_action) and not Input.is_action_pressed(down_action):
			_direction.y = 0

func _process(delta):
	if privot:
		_update_distance()
	if mouselook:
		_update_mouselook()
	if movement:
		_update_movement(delta)

func _physics_process(delta):
	# Called when collision are enabled
	_update_distance()
	if mouselook:
		_update_mouselook()

	var space_state = get_world().get_direct_space_state()
	var obstacle = space_state.intersect_ray(privot.get_translation(),  get_translation())
	if not obstacle.empty():
		set_translation(obstacle.position)

func _update_movement(delta):
	var offset = max_speed * acceleration * _direction
	
	_speed.x = clamp(_speed.x + offset.x, -max_speed.x, max_speed.x)
	_speed.y = clamp(_speed.y + offset.y, -max_speed.y, max_speed.y)
	_speed.z = clamp(_speed.z + offset.z, -max_speed.z, max_speed.z)
	
	# Apply deceleration if no input
	if _direction.x == 0:
		_speed.x *= (1.0 - deceleration)
	if _direction.y == 0:
		_speed.y *= (1.0 - deceleration)
	if _direction.z == 0:
		_speed.z *= (1.0 - deceleration)

	if local:
		translate(_speed * delta)
	else:
		global_translate(_speed * delta)

func _update_mouselook():
	_mouse_position *= sensitivity
	_yaw = _yaw * smoothness + _mouse_position.x * (1.0 - smoothness)
	_pitch = _pitch * smoothness + _mouse_position.y * (1.0 - smoothness)
	_mouse_position = Vector2(0, 0)

	if yaw_limit < 360:
		_yaw = clamp(_yaw, -yaw_limit - _total_yaw, yaw_limit - _total_yaw)
	if pitch_limit < 360:
		_pitch = clamp(_pitch, -pitch_limit - _total_pitch, pitch_limit - _total_pitch)

	_total_yaw += _yaw
	_total_pitch += _pitch

	if privot:
		var target = privot.get_translation()
		var offset = get_translation().distance_to(target)

		set_translation(target)
		rotate_y(deg2rad(-_yaw))
		rotate_object_local(Vector3(1,0,0), deg2rad(-_pitch))
		translate(Vector3(0.0, 0.0, offset))

		if rotate_privot:
			privot.rotate_y(deg2rad(-_yaw))
	else:
		rotate_y(deg2rad(-_yaw))
		rotate_object_local(Vector3(1,0,0), deg2rad(-_pitch))

func _update_distance():
	var t = privot.get_translation()
	t.z -= distance
	set_translation(t)

func _update_process_func():
	# Use physics process if collision are enabled
	if collisions and privot:
		set_physics_process(true)
		set_process(false)
	else:
		set_physics_process(false)
		set_process(true)

func _check_actions(actions=[]):
	if OS.is_debug_build():
		for action in actions:
			if not InputMap.has_action(action):
				print('WARNING: No action "' + action + '"')

func set_privot(value):
	privot = value
	# TODO: fix parenting.
#	if privot:
#		if get_parent():
#			get_parent().remove_child(self)
#		privot.add_child(self)
	_update_process_func()

func set_collisions(value):
	collisions = value
	_update_process_func()

func set_enabled(value):
	enabled = value
	if enabled:
		Input.set_mouse_mode(mouse_mode)
		set_process_input(true)
		_update_process_func()
	else:
		set_process(false)
		set_process_input(false)
		set_physics_process(false)

func set_smoothness(value):
	smoothness = clamp(value, 0.001, 0.999)

func set_distance(value):
	distance = max(0, value)
