all: check
	cd game && make ; cd -
run:
	cd game && make run ; cd -
watch:
	cd game && make watch ; cd -

.PHONY: check
check:
ifndef TELEMETRY
	@cat  "./Consent.md"
	@read line; if [ $$line == "n" ]; then echo "Please set TELEMETRY='--flag simula:no-tracking' and re-run make."; exit 1 ; fi
  # @read line; if [ $$line == "n" ]; then export TELEMETRY='--flag simula:no-tracking'; fi # Doesn't work
endif
