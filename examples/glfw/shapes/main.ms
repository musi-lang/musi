// 2D Shapes Demo
// Draws colored rectangles and circles in 2D orthographic view.

import { musi_glfw_init, musi_glfw_terminate, musi_glfw_create_window,
         musi_glfw_destroy, musi_glfw_make_current, musi_glfw_swap_interval,
         musi_glfw_swap, musi_glfw_poll_events, musi_glfw_should_close,
         musi_glfw_get_time, musi_glfw_get_key,
         musi_glfw_get_cursor_x, musi_glfw_get_cursor_y,
         musi_glfw_get_width, musi_glfw_get_height,
         musi_gl_viewport, musi_gl_clear_black, musi_gl_clear,
         musi_gl_setup_ortho, musi_gl_color3_rgb,
         musi_gl_draw_rect, musi_gl_draw_circle,
         musi_gl_color_buffer_bit, musi_glfw_key_escape
       } from "../lib/glfw";

const ok := musi_glfw_init();
if ok /= 1 then writeln("Failed to initialize GLFW");

const win := musi_glfw_create_window(640, 480);
if win = 0 then (
    writeln("Failed to create window");
    musi_glfw_terminate();
);

musi_glfw_make_current(win);
musi_glfw_swap_interval(1);

const KEY_ESCAPE := musi_glfw_key_escape();
const COLOR_BUFFER := musi_gl_color_buffer_bit();

// Colors as packed RGB integers
const RED := 0xFF0000;
const GREEN := 0x00FF00;
const BLUE := 0x0000FF;
const YELLOW := 0xFFFF00;
const CYAN := 0x00FFFF;
const MAGENTA := 0xFF00FF;

while musi_glfw_should_close(win) = 0 loop (
    const t := musi_glfw_get_time();
    const width := musi_glfw_get_width(win);
    const height := musi_glfw_get_height(win);
    const mx := musi_glfw_get_cursor_x(win);
    const my := musi_glfw_get_cursor_y(win);

    musi_gl_viewport(width, height);
    musi_gl_clear_black();
    musi_gl_clear(COLOR_BUFFER);
    musi_gl_setup_ortho(width, height);

    // Static rectangles
    musi_gl_color3_rgb(RED);
    musi_gl_draw_rect(50.0, 50.0);

    musi_gl_color3_rgb(GREEN);
    musi_gl_draw_rect(150.0, 50.0);

    musi_gl_color3_rgb(BLUE);
    musi_gl_draw_rect(250.0, 50.0);

    // Animated circle (follows sine wave)
    const cx := 320.0 + 100.0 * sin(t * 2.0);
    const cy := 240.0 + 50.0 * sin(t * 3.0);
    musi_gl_color3_rgb(YELLOW);
    musi_gl_draw_circle(cx, cy);

    // Circle follows mouse cursor
    musi_gl_color3_rgb(CYAN);
    musi_gl_draw_circle(int_to_float(mx), int_to_float(my));

    // Bouncing rectangle
    const rx := 100.0 + 200.0 * (0.5 + 0.5 * sin(t));
    const ry := 350.0;
    musi_gl_color3_rgb(MAGENTA);
    musi_gl_draw_rect(rx, ry);

    musi_glfw_swap(win);
    musi_glfw_poll_events();

    if musi_glfw_get_key(win, KEY_ESCAPE) /= 0 then (
        musi_glfw_destroy(win);
        musi_glfw_terminate();
    );
);

musi_glfw_destroy(win);
musi_glfw_terminate();
