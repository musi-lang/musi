/*
 * GLFW + OpenGL shim — wraps pointer-heavy APIs with i64/f64-only ABI
 * for Musi's FFI. Includes basic OpenGL 1.x immediate-mode functions.
 *
 * Build (macOS):
 *   cc -shared -fPIC -o libmusi_glfw.dylib glfw_shim.c \
 *      -L/opt/homebrew/lib -L/usr/local/lib -lglfw -framework OpenGL \
 *      -DGL_SILENCE_DEPRECATION
 */

#include "include/GLFW/glfw3.h"
#include <stdint.h>
#include <math.h>

#ifdef __APPLE__
#define GL_SILENCE_DEPRECATION
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#endif

// -- GLFW Core --

int64_t musi_glfw_init(void) { return (int64_t)glfwInit(); }
void musi_glfw_terminate(void) { glfwTerminate(); }

int64_t musi_glfw_create_window(int64_t w, int64_t h) {
  return (int64_t)(uintptr_t)glfwCreateWindow((int)w, (int)h, "Musi", NULL,
                                              NULL);
}

void musi_glfw_destroy(int64_t win) {
  glfwDestroyWindow((GLFWwindow *)(uintptr_t)win);
}

void musi_glfw_make_current(int64_t win) {
  glfwMakeContextCurrent((GLFWwindow *)(uintptr_t)win);
}

void musi_glfw_swap_interval(int64_t interval) {
  glfwSwapInterval((int)interval);
}

void musi_glfw_swap(int64_t win) {
  glfwSwapBuffers((GLFWwindow *)(uintptr_t)win);
}

void musi_glfw_poll_events(void) { glfwPollEvents(); }

int64_t musi_glfw_should_close(int64_t win) {
  return (int64_t)glfwWindowShouldClose((GLFWwindow *)(uintptr_t)win);
}

double musi_glfw_get_time(void) { return glfwGetTime(); }

int64_t musi_glfw_get_key(int64_t win, int64_t key) {
  return (int64_t)glfwGetKey((GLFWwindow *)(uintptr_t)win, (int)key);
}

int64_t musi_glfw_get_cursor_x(int64_t win) {
  double x, y;
  glfwGetCursorPos((GLFWwindow *)(uintptr_t)win, &x, &y);
  return (int64_t)x;
}

int64_t musi_glfw_get_cursor_y(int64_t win) {
  double x, y;
  glfwGetCursorPos((GLFWwindow *)(uintptr_t)win, &x, &y);
  return (int64_t)y;
}

int64_t musi_glfw_get_width(int64_t win) {
  int w, h;
  glfwGetFramebufferSize((GLFWwindow *)(uintptr_t)win, &w, &h);
  return (int64_t)w;
}

int64_t musi_glfw_get_height(int64_t win) {
  int w, h;
  glfwGetFramebufferSize((GLFWwindow *)(uintptr_t)win, &w, &h);
  return (int64_t)h;
}

// -- OpenGL Wrappers --

void musi_gl_viewport(int64_t w, int64_t h) {
  glViewport(0, 0, (GLsizei)w, (GLsizei)h);
}

void musi_gl_clear_color(double r, double g) {
  // Note: only 2 params due to FFI limit, b=0 a=1
  glClearColor((GLfloat)r, (GLfloat)g, 0.0f, 1.0f);
}

void musi_gl_clear_black(void) { glClearColor(0.0f, 0.0f, 0.0f, 1.0f); }

void musi_gl_clear(int64_t mask) { glClear((GLbitfield)mask); }

void musi_gl_setup_projection(double aspect) {
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(65.0, aspect, 1.0, 100.0);
}

void musi_gl_setup_ortho(int64_t w, int64_t h) {
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, (double)w, (double)h, 0, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

void musi_gl_setup_camera(void) {
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0.0, 1.0, 0.0,  // Eye
            0.0, 20.0, 0.0, // Center
            0.0, 0.0, 1.0); // Up
}

void musi_gl_translate_z(double z) { glTranslatef(0.0f, (GLfloat)z, 0.0f); }

void musi_gl_rotate_z(double angle) {
  glRotatef((GLfloat)angle, 0.0f, 0.0f, 1.0f);
}

void musi_gl_rotate_x(double angle) {
  glRotatef((GLfloat)angle, 1.0f, 0.0f, 0.0f);
}

void musi_gl_rotate_y(double angle) {
  glRotatef((GLfloat)angle, 0.0f, 1.0f, 0.0f);
}

void musi_gl_color3(double r, double g) {
  // Note: b=0 due to 2-param FFI limit; use musi_gl_color3b for blue
  glColor3f((GLfloat)r, (GLfloat)g, 0.0f);
}

void musi_gl_color3_rgb(int64_t rgb) {
  // Pack RGB into single int: 0xRRGGBB
  float r = ((rgb >> 16) & 0xFF) / 255.0f;
  float g = ((rgb >> 8) & 0xFF) / 255.0f;
  float b = (rgb & 0xFF) / 255.0f;
  glColor3f(r, g, b);
}

void musi_gl_draw_triangle(void) {
  glBegin(GL_TRIANGLES);
  glColor3f(1.0f, 0.0f, 0.0f);
  glVertex3f(-5.0f, 0.0f, -4.0f);
  glColor3f(0.0f, 1.0f, 0.0f);
  glVertex3f(5.0f, 0.0f, -4.0f);
  glColor3f(0.0f, 0.0f, 1.0f);
  glVertex3f(0.0f, 0.0f, 6.0f);
  glEnd();
}

// -- 2D Shape Drawing --

void musi_gl_draw_rect(double x, double y) {
  // Draw 50x50 rect at (x, y)
  glBegin(GL_QUADS);
  glVertex2f((GLfloat)x, (GLfloat)y);
  glVertex2f((GLfloat)(x + 50), (GLfloat)y);
  glVertex2f((GLfloat)(x + 50), (GLfloat)(y + 50));
  glVertex2f((GLfloat)x, (GLfloat)(y + 50));
  glEnd();
}

void musi_gl_draw_rect_size(int64_t x, int64_t y) {
  // Size passed via second call
  static int64_t saved_x, saved_y;
  saved_x = x;
  saved_y = y;
}

void musi_gl_draw_circle(double cx, double cy) {
  // Draw circle approximation (16 segments)
  const int segments = 16;
  const double radius = 30.0;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f((GLfloat)cx, (GLfloat)cy);
  for (int i = 0; i <= segments; i++) {
    double angle = 2.0 * 3.14159265 * i / segments;
    glVertex2f((GLfloat)(cx + radius * cos(angle)),
               (GLfloat)(cy + radius * sin(angle)));
  }
  glEnd();
}

// -- 3D Cube Drawing --

static const GLfloat cube_vertices[8][3] = {
    {-1, -1, -1}, {1, -1, -1}, {1, 1, -1}, {-1, 1, -1},
    {-1, -1, 1},  {1, -1, 1},  {1, 1, 1},  {-1, 1, 1}};

static const int cube_edges[12][2] = {{0, 1}, {1, 2}, {2, 3}, {3, 0},
                                       {4, 5}, {5, 6}, {6, 7}, {7, 4},
                                       {0, 4}, {1, 5}, {2, 6}, {3, 7}};

void musi_gl_setup_cube_camera(void) {
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0.0, 0.0, 5.0,  // Eye
            0.0, 0.0, 0.0,  // Center
            0.0, 1.0, 0.0); // Up
}

void musi_gl_draw_cube_wireframe(void) {
  glBegin(GL_LINES);
  for (int i = 0; i < 12; i++) {
    int v1 = cube_edges[i][0];
    int v2 = cube_edges[i][1];
    glVertex3fv(cube_vertices[v1]);
    glVertex3fv(cube_vertices[v2]);
  }
  glEnd();
}

void musi_gl_draw_cube_solid(void) {
  // Front face
  glBegin(GL_QUADS);
  glColor3f(1.0f, 0.0f, 0.0f); // Red
  glVertex3f(-1, -1, 1);
  glVertex3f(1, -1, 1);
  glVertex3f(1, 1, 1);
  glVertex3f(-1, 1, 1);

  glColor3f(0.0f, 1.0f, 0.0f); // Green
  glVertex3f(-1, -1, -1);
  glVertex3f(-1, 1, -1);
  glVertex3f(1, 1, -1);
  glVertex3f(1, -1, -1);

  glColor3f(0.0f, 0.0f, 1.0f); // Blue
  glVertex3f(-1, 1, -1);
  glVertex3f(-1, 1, 1);
  glVertex3f(1, 1, 1);
  glVertex3f(1, 1, -1);

  glColor3f(1.0f, 1.0f, 0.0f); // Yellow
  glVertex3f(-1, -1, -1);
  glVertex3f(1, -1, -1);
  glVertex3f(1, -1, 1);
  glVertex3f(-1, -1, 1);

  glColor3f(1.0f, 0.0f, 1.0f); // Magenta
  glVertex3f(1, -1, -1);
  glVertex3f(1, 1, -1);
  glVertex3f(1, 1, 1);
  glVertex3f(1, -1, 1);

  glColor3f(0.0f, 1.0f, 1.0f); // Cyan
  glVertex3f(-1, -1, -1);
  glVertex3f(-1, -1, 1);
  glVertex3f(-1, 1, 1);
  glVertex3f(-1, 1, -1);
  glEnd();
}

void musi_gl_enable_depth(void) {
  glEnable(GL_DEPTH_TEST);
}

// -- GL Constants --

int64_t musi_gl_color_buffer_bit(void) { return GL_COLOR_BUFFER_BIT; }
int64_t musi_gl_depth_buffer_bit(void) { return GL_DEPTH_BUFFER_BIT; }

// -- GLFW Key Constants --

int64_t musi_glfw_key_escape(void) { return GLFW_KEY_ESCAPE; }
int64_t musi_glfw_key_space(void) { return GLFW_KEY_SPACE; }
int64_t musi_glfw_key_up(void) { return GLFW_KEY_UP; }
int64_t musi_glfw_key_down(void) { return GLFW_KEY_DOWN; }
int64_t musi_glfw_key_left(void) { return GLFW_KEY_LEFT; }
int64_t musi_glfw_key_right(void) { return GLFW_KEY_RIGHT; }
int64_t musi_glfw_key_w(void) { return GLFW_KEY_W; }
int64_t musi_glfw_key_a(void) { return GLFW_KEY_A; }
int64_t musi_glfw_key_s(void) { return GLFW_KEY_S; }
int64_t musi_glfw_key_d(void) { return GLFW_KEY_D; }
