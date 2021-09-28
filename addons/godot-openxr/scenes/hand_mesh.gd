tool
extends Spatial

enum MOTION_RANGE {
	UNUBSTRUCTED = 0,
	CONFORM_TO_CONTROLLER = 1
}

export (MOTION_RANGE) var motion_range setget set_motion_range
export (Texture) var albedo_texture setget set_albedo_texture
export (Texture) var normal_texture setget set_normal_texture

var material : SpatialMaterial

func set_motion_range(value):
	motion_range = value
	if is_inside_tree():
		_update_motion_range()

func _update_motion_range():
	# for some reason not consistantly named between the two hands..
	if $HandModel.find_node("Armature001"):
		$HandModel/Armature001/Skeleton.motion_range = motion_range
	else:
		$HandModel/Armature/Skeleton.motion_range = motion_range

func set_albedo_texture(value):
	albedo_texture = value
	if is_inside_tree():
		_update_albedo_texture()

func _update_albedo_texture():
	if material:
		material.albedo_texture = albedo_texture

func set_normal_texture(value):
	normal_texture = value
	if is_inside_tree():
		_update_normal_texture()

func _update_normal_texture():
	if material:
		material.normal_texture = normal_texture

# Called when the node enters the scene tree for the first time.
func _ready():
	if $HandModel.find_node("Armature001"):
		material = $HandModel/Armature001/Skeleton/vr_glove_left_slim.mesh.surface_get_material(0)
	else:
		material = $HandModel/Armature/Skeleton/vr_glove_right_slim.mesh.surface_get_material(0)

	_update_motion_range()
	_update_albedo_texture()
	_update_normal_texture()
