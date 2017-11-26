clean:
	rm -rf .stack-work simula-*/.stack-work

init:
	git submodule update --init --recursive

pullall:
	git pull --ff-only && git submodule update --init --recursive
