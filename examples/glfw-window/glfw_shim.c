/*
 * GLFW shim — wraps GLFW's pointer-heavy API with an int64_t-only ABI
 * so Musi's FFI (which speaks Int / i64) can call it directly.
 *
 * Build (macOS):
 *   cc -shared -o libmusi_glfw.dylib glfw_shim.c -Iinclude $(pkg-config --libs
 * glfw3) # or, if pkg-config is not available: cc -shared -o libmusi_glfw.dylib
 * glfw_shim.c -Iinclude -lglfw
 *
 * Build (Linux):
 *   cc -shared -fPIC -o libmusi_glfw.so glfw_shim.c -Iinclude $(pkg-config
 * --libs glfw3)
 */

#include "include/GLFW/glfw3.h"
#include <stdint.h>

int64_t musi_glfw_init(void) { return (int64_t)glfwInit(); }

int64_t musi_glfw_create_window(int64_t w, int64_t h) {
  return (int64_t)(uintptr_t)glfwCreateWindow((int)w, (int)h, "Musi", NULL,
                                              NULL);
}

void musi_glfw_poll_events(void) { glfwPollEvents(); }

int64_t musi_glfw_should_close(int64_t win) {
  return (int64_t)glfwWindowShouldClose((GLFWwindow *)(uintptr_t)win);
}

void musi_glfw_destroy(int64_t win) {
  glfwDestroyWindow((GLFWwindow *)(uintptr_t)win);
}

void musi_glfw_terminate(void) { glfwTerminate(); }

void musi_glfw_swap(int64_t win) {
  glfwSwapBuffers((GLFWwindow *)(uintptr_t)win);
}
