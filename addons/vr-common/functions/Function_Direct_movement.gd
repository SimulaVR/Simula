extends Spatial

# some handy nodes...
export (NodePath) var origin = null
export (NodePath) var camera = null

# size of our player
export var player_height = 1.8 setget set_player_height, get_player_height
export var player_radius = 0.4 setget set_player_radius, get_player_radius

# to combat motion sickness we'll 'step' our left/right turning
export var turn_delay = 0.2
export var turn_angle = 20.0
export var max_speed = 5.0
export var drag_factor = 0.1

var turn_step = 0.0
var origin_node = null
var camera_node = null
var velocity = Vector3(0.0, 0.0, 0.0)
var gravity = -30.0
onready var collision_shape = get_node("KinematicBody/CollisionShape")
onready var tail = get_node("KinematicBody/Tail")

func get_player_height():
	return player_height

func set_player_height(p_height):
	player_height = p_height
	
	if collision_shape:
		# for some reason collision shape height measurement is half up, half down from center 
		collision_shape.shape.height = (player_height / 2.0)
		collision_shape.translation = Vector3(0.0, player_height / 2.0, 0.0)

func get_player_radius():
	return player_radius

func set_player_radius(p_radius):
	player_radius = p_radius
	
	if collision_shape:
		collision_shape.shape.height = (player_height / 2.0)
		collision_shape.shape.radius = player_radius

func _ready():
	origin_node = get_node(origin)
	camera_node = get_node(camera)
	
	set_player_height(player_height)
	set_player_radius(player_radius)

func _physics_process(delta):
	# We should be the child or the controller on which the teleport is implemented
	var controller = get_parent()
	if controller.get_is_active():
		var left_right = controller.get_joystick_axis(0)
		var forwards_backwards = controller.get_joystick_axis(1)
		
		################################################################
		# first process turning, no problems there :)
		if (abs(left_right) > 0.1):
			if left_right > 0.0:
				if turn_step < 0.0:
					# reset step
					turn_step = 0
			
				turn_step += left_right * delta
			else:
				if turn_step > 0.0:
					# reset step
					turn_step = 0
			
				turn_step += left_right * delta
		
			if abs(turn_step) > turn_delay:
				# we rotate around our Camera, but we adjust our origin, so we need a little bit of trickery
				var t1 = Transform()
				var t2 = Transform()
				var rot = Transform()
			
				t1.origin = -camera_node.transform.origin
				t2.origin = camera_node.transform.origin
			
				# Rotating
				while abs(turn_step) > turn_delay:
					if (turn_step > 0.0):
						rot = rot.rotated(Vector3(0.0,-1.0,0.0),turn_angle * PI / 180.0)
						turn_step -= turn_delay
					else:
						rot = rot.rotated(Vector3(0.0,1.0,0.0),turn_angle * PI / 180.0)
						turn_step += turn_delay
			
				origin_node.transform *= t2 * rot * t1
		else:
			turn_step = 0.0
		
		################################################################
		# now we do our movement
		# We start with placing our KinematicBody in the right place
		# by centering it on the camera but placing it on the ground
		var new_transform = $KinematicBody.global_transform
		var camera_transform = camera_node.global_transform
		new_transform.origin = camera_transform.origin
		new_transform.origin.y = origin_node.global_transform.origin.y
		$KinematicBody.global_transform = new_transform
		
		# we'll handle gravity separately
		var gravity_velocity = Vector3(0.0, velocity.y, 0.0)
		velocity.y = 0.0
		
		# Apply our drag
		velocity *= (1.0 - drag_factor)
		
		if (abs(forwards_backwards) > 0.1 and tail.is_colliding()):
			var dir = camera_transform.basis.z
			dir.y = 0.0
			
			velocity = dir.normalized() * -forwards_backwards * delta * max_speed * ARVRServer.world_scale
#			velocity = velocity.linear_interpolate(dir, delta * 100.0)
		
		# apply move and slide to our kinematic body
		velocity = $KinematicBody.move_and_slide(velocity, Vector3(0.0, 1.0, 0.0))
		
		# apply our gravity
		gravity_velocity.y += gravity * delta
		gravity_velocity = $KinematicBody.move_and_slide(gravity_velocity, Vector3(0.0, 1.0, 0.0))
		velocity.y = gravity_velocity.y
		
		# now use our new position to move our origin point
		var movement = ($KinematicBody.global_transform.origin - new_transform.origin)
		origin_node.global_transform.origin += movement
		
		# Return this back to where it was so we can use its collision shape for other things too
		$KinematicBody.global_transform.origin = new_transform.origin
		
		# We can't use move and collide here because we're moving our world center and thus our kinematic body indirectly.
		# Need to improve on that...
		# origin_node.translation -= dir.normalized() * delta * forwards_backwards * max_speed * ARVRServer.world_scale;
