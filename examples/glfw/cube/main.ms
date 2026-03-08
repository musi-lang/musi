// 3D Rotating Cube Demo
// A colorful rotating cube with depth testing.

import { musi_glfw_init, musi_glfw_terminate, musi_glfw_create_window,
         musi_glfw_destroy, musi_glfw_make_current, musi_glfw_swap_interval,
         musi_glfw_swap, musi_glfw_poll_events, musi_glfw_should_close,
         musi_glfw_get_time, musi_glfw_get_key,
         musi_glfw_get_width, musi_glfw_get_height,
         musi_gl_viewport, musi_gl_clear_black, musi_gl_clear,
         musi_gl_setup_projection, musi_gl_setup_cube_camera,
         musi_gl_rotate_x, musi_gl_rotate_y, musi_gl_enable_depth,
         musi_gl_draw_cube_solid,
         musi_gl_color_buffer_bit, musi_gl_depth_buffer_bit,
         musi_glfw_key_escape, musi_glfw_key_space
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
musi_gl_enable_depth();

const KEY_ESCAPE := musi_glfw_key_escape();
const KEY_SPACE := musi_glfw_key_space();
const COLOR_BUFFER := musi_gl_color_buffer_bit();
const DEPTH_BUFFER := musi_gl_depth_buffer_bit();

writeln("Press SPACE to pause/resume rotation.");
writeln("Press ESC to exit.");

var paused := 0;
var angle_x := 0.0;
var angle_y := 0.0;
var space_was_pressed := 0;

while musi_glfw_should_close(win) = 0 loop (
    const t := musi_glfw_get_time();
    const width := musi_glfw_get_width(win);
    var height := musi_glfw_get_height(win);
    if height < 1 then height <- 1;

    // Toggle pause on space press (edge detection)
    const space_pressed := musi_glfw_get_key(win, KEY_SPACE);
    if space_pressed /= 0 then (
        if space_was_pressed = 0 then (
            if paused = 0 then paused <- 1
            else paused <- 0;
        );
        space_was_pressed <- 1;
    ) else space_was_pressed <- 0;

    // Update rotation angles
    if paused = 0 then (
        angle_x <- t * 50.0;
        angle_y <- t * 30.0;
    );

    musi_gl_viewport(width, height);
    musi_gl_clear_black();
    musi_gl_clear(COLOR_BUFFER + DEPTH_BUFFER);

    const aspect := int_to_float(width) / int_to_float(height);
    musi_gl_setup_projection(aspect);
    musi_gl_setup_cube_camera();

    musi_gl_rotate_x(angle_x);
    musi_gl_rotate_y(angle_y);

    musi_gl_draw_cube_solid();

    musi_glfw_swap(win);
    musi_glfw_poll_events();

    if musi_glfw_get_key(win, KEY_ESCAPE) /= 0 then (
        musi_glfw_destroy(win);
        musi_glfw_terminate();
    );
);

musi_glfw_destroy(win);
musi_glfw_terminate();
