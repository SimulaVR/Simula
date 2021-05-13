extends Spatial

signal pointer_pressed(on, at)
signal pointer_released(on, at)
signal pointer_moved(on, from, to)

signal pointer_entered(body)
signal pointer_exited(body)

# enum our buttons, should find a way to put this more central
enum Buttons {
	VR_BUTTON_BY = 1,
	VR_GRIP = 2,
	VR_BUTTON_3 = 3,
	VR_BUTTON_4 = 4,
	VR_BUTTON_5 = 5,
	VR_BUTTON_6 = 6,
	VR_BUTTON_AX = 7,
	VR_BUTTON_8 = 8,
	VR_BUTTON_9 = 9,
	VR_BUTTON_10 = 10,
	VR_BUTTON_11 = 11,
	VR_BUTTON_12 = 12,
	VR_BUTTON_13 = 13,
	VR_PAD = 14,
	VR_TRIGGER = 15,
	VR_ACTION = 255
}

export var enabled = true setget set_enabled
export var show_laser = true setget set_show_laser
export var show_target = false
export var ducktyped_body = true
export (Buttons) var active_button = Buttons.VR_TRIGGER
export var action = ""
export var y_offset = -0.05 setget set_y_offset
export var distance = 10 setget set_distance
export (int, LAYERS_3D_PHYSICS) var collision_mask = 15 setget set_collision_mask
export var collide_with_bodies = true setget set_collide_with_bodies
export var collide_with_areas = false setget set_collide_with_areas

var target = null
var last_target = null
var last_collided_at = Vector3(0, 0, 0)

var ws = 1.0

func set_enabled(p_enabled):
	enabled = p_enabled
	
	# this gets called before our scene is ready, we'll call this again in _ready to enable this
	if is_inside_tree():
		$Laser.visible = p_enabled and show_laser
		$RayCast.enabled = p_enabled

func set_show_laser(p_show):
	show_laser = p_show
	if is_inside_tree():
		$Laser.visible = enabled and show_laser

func set_y_offset(p_offset):
	y_offset = p_offset
	if is_inside_tree():
		$Laser.translation.y = y_offset * ws
		$RayCast.translation.y = y_offset * ws

func set_collision_mask(p_new_mask):
	collision_mask = p_new_mask
	if is_inside_tree():
		$RayCast.collision_mask = collision_mask

func set_distance(p_new_value):
	distance = p_new_value
	if is_inside_tree():
		$Laser.mesh.size.z = distance
		$Laser.translation.z = distance * -0.5
		$RayCast.cast_to.z = -distance

func set_collide_with_bodies(p_new_value : bool):
	collide_with_bodies = p_new_value
	if is_inside_tree():
		$RayCast.collide_with_bodies = collide_with_bodies

func set_collide_with_areas(p_new_value : bool):
	collide_with_areas = p_new_value
	if is_inside_tree():
		$RayCast.collide_with_areas = collide_with_areas

func _button_pressed():
	if $RayCast.is_colliding():
		target = $RayCast.get_collider()
		last_collided_at = $RayCast.get_collision_point()
		
		emit_signal("pointer_pressed", target, last_collided_at)
		
		if ducktyped_body and target.has_method("pointer_pressed"):
			target.pointer_pressed(last_collided_at)

func _button_released():
	if target:
		emit_signal("pointer_released", target, last_collided_at)
		
		if ducktyped_body and target.has_method("pointer_released"):
			target.pointer_released(last_collided_at)
		
		# unset target
		target = null
		last_collided_at = Vector3(0, 0, 0)

func _on_button_pressed(p_button):
	if p_button == active_button and enabled:
		_button_pressed()

func _on_button_release(p_button):
	if p_button == active_button and target:
		_button_released()

func _ready():
	ws = ARVRServer.world_scale
	
	if active_button != Buttons.VR_ACTION:
		# Get button press feedback from our parent (should be an ARVRController)
		get_parent().connect("button_pressed", self, "_on_button_pressed")
		get_parent().connect("button_release", self, "_on_button_release")
	
	# init our state
	set_y_offset(y_offset)
	set_distance(distance)
	set_collision_mask(collision_mask)
	set_show_laser(show_laser)
	set_collide_with_bodies(collide_with_bodies)
	set_collide_with_areas(collide_with_areas)
	set_enabled(enabled)

func _process(delta):
	if !is_inside_tree():
		return
	
	if active_button == Buttons.VR_ACTION and action != "":
		if Input.is_action_just_pressed(action):
			_button_pressed()
		elif !Input.is_action_pressed(action) and target:
			_button_released()
	
	var new_ws = ARVRServer.world_scale
	if (ws != new_ws):
		ws = new_ws
		set_y_offset(y_offset)
	
	if enabled and $RayCast.is_colliding():
		var new_at = $RayCast.get_collision_point()
		
		if is_instance_valid(target):
			# if target is set our mouse must be down, we keep "focus" on our target
			if new_at != last_collided_at:
				emit_signal("pointer_moved", target, last_collided_at, new_at)
				
				if ducktyped_body and target.has_method("pointer_moved"):
					target.pointer_moved(last_collided_at, new_at)
		else:
			var new_target = $RayCast.get_collider()

			# are we pointing to a new target?
			if new_target != last_target:
				# exit the old
				if is_instance_valid(last_target):
					emit_signal("pointer_exited", last_target)

					if ducktyped_body and last_target.has_method("pointer_exited"):
						last_target.pointer_exited()

				# enter the new
				if is_instance_valid(new_target):
					emit_signal("pointer_entered", new_target)

					if ducktyped_body and new_target.has_method("pointer_entered"):
						new_target.pointer_entered()

				last_target = new_target

			if new_at != last_collided_at:
				emit_signal("pointer_moved", new_target, last_collided_at, new_at)

				if ducktyped_body and new_target.has_method("pointer_moved"):
					new_target.pointer_moved(last_collided_at, new_at)

		if last_target and show_target:
			$Target.global_transform.origin = last_collided_at
			$Target.visible = true

		# remember our new position
		last_collided_at = new_at
	else:
		if is_instance_valid(last_target):
			emit_signal("pointer_exited", last_target)
			
			if ducktyped_body and last_target.has_method("pointer_exited"):
				last_target.pointer_exited()
		
		last_target = null
		$Target.visible = false
