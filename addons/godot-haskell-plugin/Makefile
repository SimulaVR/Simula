LIBFILE = $(shell stack path --local-install-root)/lib/libgodot-haskell-plugin.so
PROJECTROOT = $(shell stack path --project-root)
GODOTPROJECT = $(PROJECTROOT)/../../
all:
	cd include && make xdg-shell-protocol.h
	cd cbits && make xdg-shell-protocol.c
	stack clean godot-haskell-plugin 
	stack build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
.PHONY: nix
nix:
	cd include && make nix
	cd cbits && make nix
run:
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	godot --path $(GODOTPROJECT)
debug-run:
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	gdb --args godot --path $(GODOTPROJECT)
watch:
	stack build --file-watch --fast --exec "cp $(LIBFILE) $(PROJECTROOT)"
clean:
	cd include && make clean
	cd cbits && make clean