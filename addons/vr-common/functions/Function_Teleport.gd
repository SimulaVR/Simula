extends KinematicBody
# should really change this to Spatial once #17401 is resolved

# Add this scene as a sub scene of your ARVRController node to implement a teleport function on that controller.
# Then set origin to your ARVROrigin node

export (NodePath) var origin = null
export (Color) var can_teleport_color = Color(0.0, 1.0, 0.0, 1.0)
export (Color) var cant_teleport_color = Color(1.0, 0.0, 0.0, 1.0)
export (Color) var no_collision_color = Color(45.0 / 255.0, 80.0 / 255.0, 220.0 / 255.0, 1.0)
export var player_height = 1.8 setget set_player_height, get_player_height
export var player_radius = 0.4 setget set_player_radius, get_player_radius
export var strength = 5.0
# once this is no longer a kinematic body, we'll need this..
# export var collision_mask = 1

onready var ws = ARVRServer.world_scale
var origin_node = null
var is_on_floor = true
var is_teleporting = false
var can_teleport = true
var teleport_rotation = 0.0;
var floor_normal = Vector3(0.0, 1.0, 0.0)
var last_target_transform = Transform()
var collision_shape = null
var step_size = 0.5

# By default we show a capsule to indicate where the player lands.
# Turn on editable children,
# hide the capsule,
# and add your own player character as child. 
onready var capsule = get_node("Target/Player_figure/Capsule")

func get_player_height():
	return player_height

func set_player_height(p_height):
	player_height = p_height
	
	if collision_shape:
		# for some reason collision shape height measurement is half up, half down from center 
		collision_shape.height = (player_height / 2.0) + 0.1
		
		if capsule:
			capsule.mesh.mid_height = player_height - (2.0 * player_radius)
			capsule.translation = Vector3(0.0, player_height/2.0, 0.0)

func get_player_radius():
	return player_radius

func set_player_radius(p_radius):
	player_radius = p_radius
	
	if collision_shape:
		collision_shape.radius = player_radius

		if capsule:
			capsule.mesh.mid_height = player_height - (2.0 * player_radius)
			capsule.mesh.radius = player_radius

func _ready():
	# And its parent should be our origin point
	origin_node = get_node(origin)

	# It's inactive when we start
	$Teleport.visible = false
	$Target.visible = false
	
	# Scale to our world scale
	$Teleport.mesh.size = Vector2(0.05 * ws, 1.0)
	$Target.mesh.size = Vector2(ws, ws)
	
	# create shape object
	collision_shape = CapsuleShape.new()
	
	# call set player to ensure our collision shape is sized
	set_player_height(player_height)
	set_player_radius(player_radius)


func _physics_process(delta):
	# We should be the child or the controller on which the teleport is implemented
	var controller = get_parent()
	
	# check if our world scale has changed..
	var new_ws = ARVRServer.world_scale
	if ws != new_ws:
		ws = new_ws
		$Teleport.mesh.size = Vector2(0.05 * ws, 1.0)
		$Target.mesh.size = Vector2(ws, ws)
	
	# button 15 is mapped to our trigger
	if controller and controller.get_is_active() and controller.is_button_pressed(15):
		if !is_teleporting:
			is_teleporting = true
			$Teleport.visible = true
			$Target.visible = true
			teleport_rotation = 0.0
		
		# get our physics engine state
		var space = PhysicsServer.body_get_space(self.get_rid())
		var state = PhysicsServer.space_get_direct_state(space)
		var query = PhysicsShapeQueryParameters.new()
		
		# init stuff about our query that doesn't change (note that safe margin and collision_mask need to change once we no longer use kinematic body)
		query.collision_mask = collision_mask
		query.margin = get_safe_margin()
		query.shape_rid = collision_shape.get_rid()
		
		# make a transform for rotating and offseting our shape, it's always lying on its side by default...
		var shape_transform = Transform(Basis(Vector3(1.0, 0.0, 0.0), deg2rad(90.0)), Vector3(0.0, player_height / 2.0, 0.0))
		
		# update location
		var teleport_global_transform = $Teleport.global_transform
		var target_global_origin = teleport_global_transform.origin
		var down = Vector3(0.0, -1.0 / ws, 0.0)
		
		############################################################
		# New teleport logic
		# We're going to use test move in steps to find out where we hit something...
		# This can be optimised loads by determining the lenght based on the angle between sections extending the length when we're in a flat part of the arch
		# Where we do get a collission we may want to fine tune the collision
		var cast_length = 0.0
		var fine_tune = 1.0
		var hit_something = false
		for i in range(1,26):
			var new_cast_length = cast_length + (step_size / fine_tune)
			var global_target = Vector3(0.0, 0.0, -new_cast_length)
			
			# our quadratic values
			var t = global_target.z / strength
			var t2 = t * t
			
			# target to world space
			global_target = teleport_global_transform.xform(global_target)
			
			# adjust for gravity
			global_target += down * t2
			
			# test our new location for collisions
			query.transform = Transform(Basis(), global_target) * shape_transform
			var cast_result = state.collide_shape(query, 10)
			if cast_result.empty():
				# we didn't collide with anything so check our next section...
				cast_length = new_cast_length
				target_global_origin = global_target
			elif (fine_tune <= 16.0):
				# try again with a small step size
				fine_tune *= 2.0
			else:
				var collided_at = target_global_origin
				if global_target.y > target_global_origin.y:
					# if we're moving up, we hit the ceiling of something, we don't really care what
					is_on_floor = false
				else:
					# now we cast a ray downwards to see if we're on a surface
					var up = Vector3(0.0, 1.0, 0.0)
					var end_pos = target_global_origin - (up * 0.1)
					var intersects = state.intersect_ray(target_global_origin, end_pos)
					if intersects.empty():
						is_on_floor = false
					else:
						# did we collide with a floor or a wall?
						floor_normal = intersects["normal"]
						var dot = floor_normal.dot(up)
						if dot > 0.9:
							is_on_floor = true
						else:
							is_on_floor = false
						
						# and return the position at which we intersected
						collided_at = intersects["position"]
				
				# we are colliding, find our if we're colliding on a wall or floor, one we can do, the other nope...
				cast_length += (collided_at - target_global_origin).length()
				target_global_origin = collided_at
				hit_something = true
				break
		
		# and just update our shader
		$Teleport.get_surface_material(0).set_shader_param("scale_t", 1.0 / strength)
		$Teleport.get_surface_material(0).set_shader_param("ws", ws)
		$Teleport.get_surface_material(0).set_shader_param("length", cast_length)
		if hit_something:
			var color = can_teleport_color
			var normal = Vector3(0.0, 1.0, 0.0)
			if is_on_floor:
				# if we're on the floor we'll reorientate our target to match.
				normal = floor_normal
				can_teleport = true
			else:
				can_teleport = false
				color = cant_teleport_color
			
			# check our axis to see if we need to rotate
			teleport_rotation += (delta * controller.get_joystick_axis(0) * -4.0)
			
			# update target and colour
			var target_basis = Basis()
			target_basis.z = Vector3(teleport_global_transform.basis.z.x, 0.0, teleport_global_transform.basis.z.z).normalized()
			target_basis.y = normal
			target_basis.x = target_basis.y.cross(target_basis.z)
			target_basis.z = target_basis.x.cross(target_basis.y)
			
			target_basis = target_basis.rotated(normal, teleport_rotation)
			last_target_transform.basis = target_basis
			last_target_transform.origin = target_global_origin + Vector3(0.0, 0.02, 0.0)
			$Target.global_transform = last_target_transform

			$Teleport.get_surface_material(0).set_shader_param("mix_color", color)
			$Target.get_surface_material(0).albedo_color = color
			$Target.visible = true
		else:
			can_teleport = false
			$Target.visible = false
			$Teleport.get_surface_material(0).set_shader_param("mix_color", no_collision_color)
	elif is_teleporting:
		if can_teleport:
			# reset our player position to center
			ARVRServer.center_on_hmd(true, true)
			
			# make our target horizontal again
			var new_transform = last_target_transform
			new_transform.basis.y = Vector3(0.0, 1.0, 0.0)
			new_transform.basis.x = new_transform.basis.y.cross(new_transform.basis.z)
			new_transform.basis.z = new_transform.basis.x.cross(new_transform.basis.y)
			
			# and change our location
			origin_node.global_transform = new_transform
		
		# and disable
		is_teleporting = false;
		$Teleport.visible = false
		$Target.visible = false

