LIBFILE = $(shell stack path --local-install-root)/lib/libgodot-haskell-plugin.so
PROJECTROOT = $(shell stack path --project-root)
GODOTPROJECT = $(PROJECTROOT)/../../
all:
	stack clean godot-haskell-plugin
	stack build
	cp $(LIBFILE) $(PROJECTROOT)
run:
	stack build
	cp $(LIBFILE) $(PROJECTROOT)
	godot --path $(GODOTPROJECT)
