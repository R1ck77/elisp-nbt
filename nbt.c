#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

static const char *HELLO_WORLD = "Hello, World (from C)!";

static emacs_value hello_world(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  return env->make_string(env, HELLO_WORLD, strlen(HELLO_WORLD));
}

static void create_hello_world(emacs_env *env)
{
  emacs_value args[2];
  args[0] = env->intern(env, "nbt-hello-world");
  args[1] = env->make_function(env, 0, 0, hello_world, "Hello, World!", NULL);
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

union float_union {
  float float_value;
  char byte_values[4];
};

static emacs_value convert_to_float(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  union float_union x;
  x.byte_values[0] = env->extract_integer(env, args[3]);
  x.byte_values[1] = env->extract_integer(env, args[2]);
  x.byte_values[2] = env->extract_integer(env, args[1]);
  x.byte_values[3] = env->extract_integer(env, args[0]);

  return env->make_float(env, x.float_value);
}

static void create_convert_to_float(emacs_env *env)
{
  emacs_value args[2];
  args[0] = env->intern(env, "nbt-convert-to-float");
  args[1] = env->make_function(env, 4, 4, convert_to_float, "Convert the bytes to float", NULL);
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

union double_union {
  double double_value;
  char byte_values[4];
};

static emacs_value convert_to_double(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  union double_union x;
  x.byte_values[0] = env->extract_integer(env, args[7]);
  x.byte_values[1] = env->extract_integer(env, args[6]);
  x.byte_values[2] = env->extract_integer(env, args[5]);
  x.byte_values[3] = env->extract_integer(env, args[4]);
  x.byte_values[4] = env->extract_integer(env, args[3]);
  x.byte_values[5] = env->extract_integer(env, args[2]);
  x.byte_values[6] = env->extract_integer(env, args[1]);
  x.byte_values[7] = env->extract_integer(env, args[0]);

  return env->make_float(env, x.double_value);
}

static void create_convert_to_double(emacs_env *env)
{
  emacs_value args[2];
  args[0] = env->intern(env, "nbt-convert-to-double");
  args[1] = env->make_function(env, 8, 8, convert_to_double, "Convert the bytes to double", NULL);
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

int emacs_module_init(struct emacs_runtime *runtime)
{
  create_hello_world(runtime->get_environment(runtime));
  create_convert_to_float(runtime->get_environment(runtime));
  create_convert_to_double(runtime->get_environment(runtime));
  return 0;
}
