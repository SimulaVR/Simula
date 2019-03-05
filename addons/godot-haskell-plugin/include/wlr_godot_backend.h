//wlr_godot_backend.h
#ifndef WLR_GODOT_BACKEND_H
#define WLR_GODOT_BACKEND_H

struct wlr_backend *wlr_godot_backend_create(struct wl_display *display,
                                             wlr_renderer_create_func_t create_renderer_func);

#endif