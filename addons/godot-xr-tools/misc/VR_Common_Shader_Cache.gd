extends Spatial

signal cooldown_finished

var countdown = 2

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	countdown = countdown - 1
	if countdown == 0:
		visible = false
		set_process(false)
		emit_signal("cooldown_finished")
