extends Spatial

####################################################################################
# and some process logic to make our hands work

func update_lengths():
	# this probably is only needed once after tracking has been on for a few frames but...
	for d in range(0,5):
		# our 5 root nodes should be our 5 fingers
		var finger = get_child(d)
		if finger and finger.get_child_count() >= 2:
			# our first node is our joint, and our second joint is the bone to that joint
			var joint = finger.get_child(0)
			var bone = finger.get_child(1)
			while joint and bone:
				var bone_len = joint.translation.length()

				# use scale and translation to place our bone
				bone.scale = Vector3(1.0, bone_len, 1.0)
				bone.translation = Vector3(0.0, 0.0, -bone_len / 2.0)

				if joint.get_child_count() >= 2:
					bone = joint.get_child(1)
					joint = joint.get_child(0)
				else:
					# the end...
					joint = null
					bone = null

func _physics_process(delta):
	# (we do this in physics because OpenXR updates positions in physics)
	update_lengths()

func _process(delta):
	# cheating because we can't extend gdns scripts...
	var parent = get_parent()
	parent.visible = parent.is_active()
