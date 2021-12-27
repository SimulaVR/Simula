tool
extends EditorPlugin

var openxr_run_select = null

func _enter_tree():
	openxr_run_select = preload("res://addons/godot-openxr/editor/OpenXRRunSelect.tscn").instance()
	add_control_to_container(CONTAINER_TOOLBAR, openxr_run_select)

func _exit_tree():
	if openxr_run_select:
		remove_control_from_container(EditorPlugin.CONTAINER_TOOLBAR, openxr_run_select)
		openxr_run_select.queue_free()
		openxr_run_select = null
