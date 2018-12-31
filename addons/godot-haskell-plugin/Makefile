LIBFILE = $(shell stack path --local-install-root)/lib/libgodot-haskell-plugin.so
PROJECTROOT = $(shell stack path --project-root)
GODOTPROJECT = $(PROJECTROOT)/../../
all:
	stack clean godot-haskell-plugin simula-wayland
	stack build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
run:
	stack --verbosity=error build godot-haskell-plugin $$TELEMETRY
	cp $(LIBFILE) $(PROJECTROOT)
	# ~/.steam/ubuntu12_32/steam-runtime/run.sh godot --path $(GODOTPROJECT)
	godot --path $(GODOTPROJECT)
watch:
	stack build --file-watch --fast --exec "cp $(LIBFILE) $(PROJECTROOT)"

clean:
	rm -r .stack-work
	rm -r simula-wayland/.stack-work