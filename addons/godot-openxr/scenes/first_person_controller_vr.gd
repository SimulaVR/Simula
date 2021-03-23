extends ARVROrigin

export (NodePath) var viewport = null

var interface : ARVRInterface

func initialise() -> bool:
	var interface = ARVRServer.find_interface("OpenXR")
	if interface and interface.initialize():
		print("OpenXR Interface initialized")

		var vp : Viewport = null
		if viewport:
			vp = get_node(viewport)

		if !vp:
			vp = get_viewport()

		# Change our viewport so it is tied to our ARVR interface and renders to our HMD
		vp.arvr = true

		# Our interface will tell us whether we should keep our render buffer in linear color space
		# If true our preview will be darker.
		vp.keep_3d_linear = $Configuration.keep_3d_linear()

		# increase our physics engine update speed
		Engine.iterations_per_second = 144

		# $Left_hand.set_physics_process(true)
		return true
	else:
		return false
