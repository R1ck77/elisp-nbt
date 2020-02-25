#include <stdlib.h>
#include <stdio.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

int emacs_module_init(struct emacs_runtime *runtime) {
  return 0;
}
