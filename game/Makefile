LIBFILE = $(shell stack path --local-install-root)/lib/libsimula.so
PROJECTROOT = $(shell stack path --project-root)
PLUGINPROJECT = ../godot-wayland
GODOT = $(PLUGINPROJECT)/godot

all: plugin
	stack build simula
	cp $(LIBFILE) $(PROJECTROOT)
run:
	stack --verbosity=error build simula
	cp $(LIBFILE) $(PROJECTROOT)
	$(GODOT) --path $(PROJECTROOT)
watch:
	stack build --file-watch --fast --exec "cp $(LIBFILE) $(PROJECTROOT)"
plugin:
	cd $(PLUGINPROJECT) && make install TARGET=$(PROJECTROOT)/addons/godot-wayland ; cd -
