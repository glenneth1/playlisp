## playlisp - M3U playlist editor
## McCLIM loads many font files; raise fd limit
SBCL := ulimit -n 8192 && sbcl --dynamic-space-size 4096

.PHONY: all gui tui build run-gui run-tui test repl clean help

help:
	@echo "playlisp build targets:"
	@echo "  make build    - Build combined binary (GUI + TUI)"
	@echo "  make gui      - Load and launch GUI interactively"
	@echo "  make tui      - Load and launch TUI interactively"
	@echo "  make test     - Run test suite"
	@echo "  make repl     - Load playlisp into REPL"
	@echo "  make clean    - Remove build artifacts"

## Build standalone binary with both GUI and TUI
build:
	$(SBCL) --non-interactive --load build.lisp

## Interactive launch (no binary needed)
## Usage: make gui [FILE=playlist.m3u]
gui:
ifdef FILE
	$(SBCL) --eval '(ql:quickload :playlisp/gui)' \
	        --eval '(playlisp-gui:run :filepath "$(FILE)" :new-process nil)'
else
	$(SBCL) --eval '(ql:quickload :playlisp/gui)' \
	        --eval '(playlisp-gui:run :new-process nil)'
endif

tui:
	$(SBCL) --eval '(ql:quickload :playlisp/mcclim)' \
	        --eval '(playlisp/src/mcclim-app:run)'

## Run the built binary
run-gui: build
	ulimit -n 8192 && ./bin/playlisp -G

run-tui: build
	./bin/playlisp -T

## Tests
test:
	sbcl --non-interactive \
	     --eval '(ql:quickload :playlisp/tests)' \
	     --eval '(asdf:test-system :playlisp)'

## Development REPL
repl:
	$(SBCL) --eval '(ql:quickload :playlisp/gui)' \
	        --eval '(in-package :playlisp-gui)'

## Clean
clean:
	rm -rf bin/playlisp *.fasl
