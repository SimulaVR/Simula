all: check
	cd addons/godot-haskell-plugin && make && cd -
run: check
	cd addons/godot-haskell-plugin && make run && cd -

.PHONY: check
check:
ifndef TELEMETRY
	@cat  "./Consent.md"
	@read line; if [ $$line == "n" ]; then echo "Please set TELEMETRY='--flag godot-haskell-plugin:no-tracking' and re-run make."; exit 1 ; fi
  # @read line; if [ $$line == "n" ]; then export TELEMETRY='--flag godot-haskell-plugin:no-tracking'; fi # Doesn't work
endif