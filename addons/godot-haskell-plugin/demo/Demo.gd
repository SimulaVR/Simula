extends Spatial

# Declare member variables here. Examples:
# var a = 2
# var b = "text"

# Called when the node enters the scene tree for the first time.
func _ready():
	var interface = ARVRServer.find_interface("OpenVR")
	if interface and interface.initialize():
		get_viewport().arvr = true
		get_viewport().hdr = false
		get_viewport().rgba8_out = true

