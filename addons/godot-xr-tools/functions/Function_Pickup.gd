extends Area

signal has_picked_up(what)
signal has_dropped

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
	VR_TRIGGER = 15
}

export var pickup_range = 0.5 setget set_pickup_range
export var impulse_factor = 1.0
export (Buttons) var pickup_button_id = Buttons.VR_GRIP
export (Buttons) var action_button_id = Buttons.VR_TRIGGER
export var max_samples = 5

var object_in_area = Array()
var closest_object = null
var picked_up_object = null

var last_position = Vector3(0.0, 0.0, 0.0)
var velocities = Array()

func set_pickup_range(new_range):
	pickup_range = new_range
	if $CollisionShape:
		$CollisionShape.shape.radius = pickup_range

func _get_velocity():
	var velocity = Vector3(0.0, 0.0, 0.0)
	var count = velocities.size()
	
	if count > 0:
		for v in velocities:
			velocity = velocity + v
		
		velocity = velocity / count
	
	return velocity

func _on_Function_Pickup_entered(object):
	# add our object to our array if required
	if object.has_method('pick_up') and object_in_area.find(object) == -1:
		object_in_area.push_back(object)
		_update_closest_object()

func _on_Function_Pickup_exited(object):
	# remove our object from our array
	if object_in_area.find(object) != -1:
		object_in_area.erase(object)
		_update_closest_object()

func _update_closest_object():
	var new_closest_obj = null
	if !picked_up_object:
		var new_closest_distance = 1000
		for o in object_in_area:
			# only check objects that aren't already picked up
			if o.is_picked_up() == false:
				var delta_pos = o.global_transform.origin - global_transform.origin
				var distance = delta_pos.length()
				if distance < new_closest_distance:
					new_closest_obj = o
					new_closest_distance = distance
	
	if closest_object != new_closest_obj:
		# remove highlight on old object
		if closest_object:
			closest_object.decrease_is_closest()
		
		# add highlight to new object
		closest_object = new_closest_obj
		if closest_object:
			closest_object.increase_is_closest()

func drop_object():
	if picked_up_object:
		# let go of this object
		picked_up_object.let_go(_get_velocity() * impulse_factor)
		picked_up_object = null
		emit_signal("has_dropped")

func _pick_up_object(p_object):
	# already holding this object, nothing to do
	if picked_up_object == p_object:
		return
	
	# holding something else? drop it
	if picked_up_object:
		drop_object()
	
	# and pick up our new object
	if p_object:
		picked_up_object = p_object
		picked_up_object.pick_up(self, get_parent())
		emit_signal("has_picked_up", picked_up_object)

func _on_button_pressed(p_button):
	if p_button == pickup_button_id:
		if picked_up_object and !picked_up_object.press_to_hold:
			drop_object()
		elif closest_object:
			_pick_up_object(closest_object)
	elif p_button == action_button_id:
		if picked_up_object and picked_up_object.has_method("action"):
			picked_up_object.action()

func _on_button_release(p_button):
	if p_button == pickup_button_id:
		if picked_up_object and picked_up_object.press_to_hold:
			drop_object()

func _ready():
	get_parent().connect("button_pressed", self, "_on_button_pressed")
	get_parent().connect("button_release", self, "_on_button_release")
	last_position = global_transform.origin
	
	# re-assign now that our collision shape has been constructed
	set_pickup_range(pickup_range)

func _process(delta):
	velocities.push_back((global_transform.origin - last_position) / delta)
	if velocities.size() > max_samples:
		velocities.pop_front()
	
	last_position = global_transform.origin
	_update_closest_object()

