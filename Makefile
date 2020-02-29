.PHONY: test clean run

EMACS=emacs

EMACS_INCLUDE_FOLDER=-I${HOME}/local/emacs/include
CFLAGS=-Wall -Wextra -O3 -fPIC $(EMACS_INCLUDE_FOLDER)

run: test

# TODO/FIXME this works only on specific systems. Check the joystick demo for a more general approach
libnbt.so: nbt.c
	gcc -shared $(CFLAGS) -o $@ $<

test: libnbt.so
	$(EMACS) --eval "(setq load-path (cons \".\" load-path))" -batch -f package-initialize -f buttercup-run-discover

clean:
	rm -f libnbt.so
