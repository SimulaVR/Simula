LIBFILE = $(shell stack path --local-install-root)/lib/libgodot-haskell-plugin.so
NIXLIBFILE = $(shell stack --nix path --local-install-root)/lib/libgodot-haskell-plugin.so
PROJECTROOT = $(shell stack path --project-root)
GODOTPROJECT = $(PROJECTROOT)/../../
all:
	mkdir -p include && cd include && make xdg-shell-protocol.h
	cd cbits && make xdg-shell-protocol.c
	stack clean godot-haskell-plugin
	stack build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
.PHONY: nix
nix:
	mkdir -p include && cd include && make nix
	cd cbits && make nix
run:
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	$(GODOTPROJECT)/bin/godot --path $(GODOTPROJECT)
editor-run:
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	$(GODOTPROJECT)/bin/godot --path $(GODOTPROJECT) -e
editor-run-gdb:
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	echo "r -C --path $(GODOTPROJECT) -e"
	gdb $(GODOTPROJECT)/bin/godot 
nix-run:
	stack --nix --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(NIXLIBFILE) $(PROJECTROOT)
	$(GODOTPROJECT)/bin/godot --path $(GODOTPROJECT)
debug-run:
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	gdb --args $(GODOTPROJECT)/bin/godot --path $(GODOTPROJECT)
local-run:
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	godot --path $(GODOTPROJECT)
local-debug-run:
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	gdb --args godot --path $(GODOTPROJECT)
watch:
	stack build --file-watch --fast --exec "cp $(LIBFILE) $(PROJECTROOT)"
clean:
	mkdir -p include && cd include && make clean
	cd cbits && make clean
