extends RigidBody

# Set hold mode
export (bool) var press_to_hold = true
export (bool) var reset_transform_on_pickup = true
export (NodePath) var center_pickup_on = null
export (NodePath) var highlight_mesh_instance = null
export  (int, LAYERS_3D_PHYSICS) var picked_up_layer = 0

# Remember some state so we can return to it when the user drops the object
onready var original_parent = get_parent()
onready var original_collision_mask = collision_mask
onready var original_collision_layer = collision_layer

onready var highlight_material = preload("res://addons/godot-xr-tools/materials/highlight.tres")
var original_materials = Array()
var highlight_mesh_instance_node : MeshInstance = null

# Who picked us up?
var picked_up_by = null
var center_pickup_on_node = null
var by_controller : ARVRController = null
var closest_count = 0

# have we been picked up?
func is_picked_up():
	if picked_up_by:
		return true

	return false

func _update_highlight():
	if highlight_mesh_instance_node:
		# if we can find a node remember which materials are currently set on each surface
		for i in range(0, highlight_mesh_instance_node.get_surface_material_count()):
			if closest_count > 0:
				highlight_mesh_instance_node.set_surface_material(i, highlight_material)
			else:
				highlight_mesh_instance_node.set_surface_material(i, original_materials[i])
	else:
		# should probably implement this in our subclass
		pass

func increase_is_closest():
	closest_count += 1
	_update_highlight()

func decrease_is_closest():
	closest_count -= 1
	_update_highlight()

func drop_and_free():
	if picked_up_by:
		picked_up_by.drop_object()

	queue_free()

# we are being picked up by...
func pick_up(by, with_controller):
	if picked_up_by == by:
		return

	if picked_up_by:
		let_go()

	# remember who picked us up
	picked_up_by = by
	by_controller = with_controller

	# turn off physics on our pickable object
	mode = RigidBody.MODE_STATIC
	collision_layer = picked_up_layer
	collision_mask = 0

	# now reparent it
	var original_transform = global_transform
	original_parent.remove_child(self)
	picked_up_by.add_child(self)

	if reset_transform_on_pickup:
		if center_pickup_on_node:
			transform = center_pickup_on_node.global_transform.inverse() * global_transform
		else:
			# reset our transform
			transform = Transform()
	else:
		# make sure we keep its original position
		global_transform = original_transform

# we are being let go
func let_go(starting_linear_velocity = Vector3(0.0, 0.0, 0.0)):
	if picked_up_by:
		# get our current global transform
		var t = global_transform

		# reparent it
		picked_up_by.remove_child(self)
		original_parent.add_child(self)

		# reposition it and apply impulse
		global_transform = t
		mode = RigidBody.MODE_RIGID
		collision_mask = original_collision_mask
		collision_layer = original_collision_layer

		# set our starting velocity
		linear_velocity = starting_linear_velocity

#		apply_impulse(Vector3(0.0, 0.0, 0.0), impulse)

		# we are no longer picked up
		picked_up_by = null
		by_controller = null

func _ready():
	if highlight_mesh_instance:
		# if we have a highlight mesh instance selected obtain our node
		highlight_mesh_instance_node = get_node(highlight_mesh_instance)
		if highlight_mesh_instance_node:
			# if we can find a node remember which materials are currently set on each surface
			for i in range(0, highlight_mesh_instance_node.get_surface_material_count()):
				original_materials.push_back(highlight_mesh_instance_node.get_surface_material(i))

	if center_pickup_on:
		# if we have center pickup on set obtain our node
		center_pickup_on_node = get_node(center_pickup_on)
