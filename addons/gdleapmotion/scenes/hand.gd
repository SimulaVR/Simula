extends Spatial

####################################################################################
# These signals are emitted by the logic below. You can subscribe on them to have
# the hands interact with the world

signal pinch_distance_changed(hand, new_value)
signal pinch_strength_changed(hand, new_value)
signal pinched(hand, is_pinched)
signal grab_strength_changed(hand, new_value)
signal grabbed(hand, is_grabbed)

####################################################################################
# These will all be updated from our GDNative module (well the set functions are)

export (float) var pinch_distance setget set_pinch_distance, get_pinch_distance
export (float) var pinch_strength setget set_pinch_strength, get_pinch_strength
export (float) var grab_strength setget set_grab_strength, get_grab_strength

var is_pinched = false
var is_grabbed = false

func set_pinch_distance(p_distance):
	if pinch_distance != p_distance:
		pinch_distance = p_distance
		emit_signal("pinch_distance_changed", self, pinch_distance)

func get_pinch_distance():
	return pinch_distance

func set_pinch_strength(p_strength):
	if pinch_strength != p_strength:
		pinch_strength = p_strength
		emit_signal("pinch_strength_changed", self, pinch_strength)
		
		if !is_pinched and pinch_strength > 0.9:
			is_pinched = true
			emit_signal("pinched", self, true)
		elif is_pinched and pinch_strength < 0.8:
			is_pinched = false
			emit_signal("pinched", self, false)

func get_pinch_strength():
	return pinch_strength

func set_grab_strength(p_strength):
	if grab_strength != p_strength:
		grab_strength = p_strength
		emit_signal("grab_strength_changed", self, grab_strength)
		
		if !is_grabbed and grab_strength > 0.9:
			is_grabbed = true
			emit_signal("grabbed", self, true)
		elif is_grabbed and grab_strength < 0.8:
			is_grabbed = false
			emit_signal("grabbed", self, false)

func get_grab_strength():
	return grab_strength

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
				bone.translation = Vector3(0.0, 0.0, bone_len / 2.0)
				
				if joint.get_child_count() >= 2:
					bone = joint.get_child(1)
					joint = joint.get_child(0)
				else:
					# the end...
					joint = null
					bone = null

func _physics_process(delta):
	# (we do this in physics because leap motion updates positions in physics)
	update_lengths()
