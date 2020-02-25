.PHONY: test clean run

EMACS_INCLUDE_FOLDER=-I${HOME}/local/emacs/include

run: test

# TODO/FIXME this works only on specific systems. Check the joystick demo for a more general approach
libnbt.so: nbt.c
	gcc $(EMACS_INCLUDE_FOLDER) -shared -fPIC -o $@ nbt.c

test: libnbt.so
	emacs --eval "(setq load-path (cons \".\" load-path))" -batch -f package-initialize -f buttercup-run-discover

clean:
	rm -f libnbt.so
