LIBFILE = $(shell stack path --local-install-root)/lib/libsimula.so
PROJECTROOT = $(shell stack path --project-root)
PLUGINPROJECT = ../godot-wayland

all: plugin
	stack build simula
	cp $(LIBFILE) $(PROJECTROOT)
run:
	stack --verbosity=error build simula
	cp $(LIBFILE) $(PROJECTROOT)
	$(PLUGINPROJECT)/godot --path $(PROJECTROOT)
watch:
	stack build --file-watch --fast --exec "cp $(LIBFILE) $(PROJECTROOT)"
plugin:
	cd $(PLUGINPROJECT) && make ; cd -
	cp $(PLUGINPROJECT)/addons/godot-wayland/libgodot-wayland.so $(PROJECTROOT)/addons/godot-wayland/
	cp $(PLUGINPROJECT)/addons/godot-wayland/*.gdn* $(PROJECTROOT)/addons/godot-wayland/
