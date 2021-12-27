extends KinematicBody

func _physics_process(delta):
	# (we do this in physics because leap motion updates positions in physics)
	
	# we're just going to update our shapes based on our bones...
	var mm_bone = get_node("../Middle/Middle_Metacarpal_Bone")
	var palm = get_node("PalmCollision")
	palm.shape.extents = Vector3(mm_bone.scale.y / 2.0, 0.005, mm_bone.scale.y / 2.0)
	translation = Vector3(0.0, 0.0, -mm_bone.scale.y / 2.0)
	
