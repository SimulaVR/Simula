//wlr_godot_backend.h
#ifndef WLR_GODOT_BACKEND_H
#define WLR_GODOT_BACKEND_H


struct wlr_godot_backend {
	struct wlr_backend backend;
	/* struct wlr_egl egl; */
	struct wlr_renderer *renderer;
	/* struct wl_display *display; */
	/* struct wl_list outputs; */
	/* struct wl_list input_devices; */
	/* struct wl_listener display_destroy; */
	/* bool started; */
};

struct wlr_backend *wlr_godot_backend_create(struct wl_display *display,
                                             wlr_renderer_create_func_t create_renderer_func);

#endif