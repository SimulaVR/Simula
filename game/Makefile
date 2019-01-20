LIBFILE = $(shell stack path --local-install-root)/lib/libsimula.so
PROJECTROOT = $(shell stack path --project-root)
GODOTPROJECT = $(PROJECTROOT)
all:
	stack build simula
	cp $(LIBFILE) $(PROJECTROOT)
run:
	stack --verbosity=error build simula
	cp $(LIBFILE) $(PROJECTROOT)
	godot --path $(GODOTPROJECT)
watch:
	stack build --file-watch --fast --exec "cp $(LIBFILE) $(PROJECTROOT)"
