.PHONY: test

test:
	emacs --eval "(setq load-path (cons \".\" load-path))" -batch -f package-initialize -f buttercup-run-discover

