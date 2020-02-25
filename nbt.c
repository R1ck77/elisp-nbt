#include <stdlib.h>
#include <stdio.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

static const char *HELLO_WORLD = "Hello, World (from C)!";

static emacs_value hello_world(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  env->make_string(env, HELLO_WORLD, strlen(HELLO_WORLD));
}

static void create_hello_world(emacs_env *env)
{
  emacs_value args[2];
  args[0] = env->intern(env, "hello-world");
  args[1] = env->make_function(env, 0, 0, hello_world, "Hello, World!", 0);
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}


int emacs_module_init(struct emacs_runtime *runtime)
{
  create_hello_world(runtime->get_environment(runtime));
  return 0;
}
