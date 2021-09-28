tool
extends OptionButton

var available_runtimes : Array = Array()
onready var platform = OS.get_name()

var home_folder = ''

func _parse_path(p_path):
	# we might want to add more stuff here at some point
	p_path = p_path.replace("~", home_folder)
	return p_path

func _update_tooltip():
	if selected > 0:
		var i = get_item_id(selected)
		hint_tooltip = _parse_path(available_runtimes[i]["path"])
	else:
		hint_tooltip = "Select the OpenXR runtime test your project with"

# Called when the node enters the scene tree for the first time.
func _ready():
	var current_runtime = 0

	# Parse the users home folder
	home_folder = OS.get_environment("HOME")
	if home_folder == '':
		home_folder = OS.get_environment("HOMEDRIVE") +  OS.get_environment("HOMEPATH")

	# read our json file, may have entries for multiple platforms, we'll filter them later
	var f = File.new()
	if (f.open("res://addons/godot-openxr/runtimes.json", File.READ)) == OK:
		var json = JSON.parse(f.get_as_text())
		available_runtimes = json.result as Array
		f.close()

	# check what our current value is
	var current_path = OS.get_environment("XR_RUNTIME_JSON")

	if available_runtimes.size() > 0:
		# reset our dropdown if applicable
		clear()
		add_item("Default", -1)

		# check which runtimes are actually available
		var dir = Directory.new()
		var index = 0
		for i in available_runtimes.size():
			var runtime = available_runtimes[i]
			var path = _parse_path(runtime["path"])
			if dir.file_exists(path):
				add_item(runtime["name"], i)
				index = index + 1
				if path == current_path:
					current_runtime = index

		selected = current_runtime
		_update_tooltip()

		visible = true
	else:
		# I guess nothing supported on this platform
		visible = false

func _on_OpenXRRunSelect_item_selected(index):
	# this need latest 3.2.4
	if index == 0:
		print("Returning to default")
		OS.set_environment("XR_RUNTIME_JSON", "")
	else:
		var i = get_item_id(index)
		var runtime = _parse_path(available_runtimes[i]["path"])
		print("Switching to " + runtime)
		OS.set_environment("XR_RUNTIME_JSON", runtime)

	_update_tooltip()
