// Spinning Triangle Demo
// A colorful RGB triangle that spins based on time and mouse position.

import { musi_glfw_init, musi_glfw_terminate, musi_glfw_create_window,
         musi_glfw_destroy, musi_glfw_make_current, musi_glfw_swap_interval,
         musi_glfw_swap, musi_glfw_poll_events, musi_glfw_should_close,
         musi_glfw_get_time, musi_glfw_get_key, musi_glfw_get_cursor_x,
         musi_glfw_get_width, musi_glfw_get_height,
         musi_gl_viewport, musi_gl_clear_black, musi_gl_clear,
         musi_gl_setup_projection, musi_gl_setup_camera,
         musi_gl_translate_z, musi_gl_rotate_z, musi_gl_draw_triangle,
         musi_gl_color_buffer_bit, musi_glfw_key_escape } from "../lib/glfw";

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

while musi_glfw_should_close(win) = 0 loop (
    const t := musi_glfw_get_time();
    const x := musi_glfw_get_cursor_x(win);
    const width := musi_glfw_get_width(win);
    var height := musi_glfw_get_height(win);
    if height < 1 then height <- 1;

    musi_gl_viewport(width, height);
    musi_gl_clear_black();
    musi_gl_clear(COLOR_BUFFER);

    const aspect := int_to_float(width) / int_to_float(height);
    musi_gl_setup_projection(aspect);
    musi_gl_setup_camera();

    musi_gl_translate_z(14.0);
    const angle := 0.3 * int_to_float(x) + t * 100.0;
    musi_gl_rotate_z(angle);

    musi_gl_draw_triangle();

    musi_glfw_swap(win);
    musi_glfw_poll_events();

    if musi_glfw_get_key(win, KEY_ESCAPE) /= 0 then (
        musi_glfw_destroy(win);
        musi_glfw_terminate();
    );
);

musi_glfw_destroy(win);
musi_glfw_terminate();
