// Basic Window Demo
// Shows keyboard input handling - press arrow keys or WASD.

import { musi_glfw_init, musi_glfw_terminate, musi_glfw_create_window,
         musi_glfw_destroy, musi_glfw_make_current, musi_glfw_swap_interval,
         musi_glfw_swap, musi_glfw_poll_events, musi_glfw_should_close,
         musi_glfw_get_key, musi_glfw_get_width, musi_glfw_get_height,
         musi_gl_viewport, musi_gl_clear_color, musi_gl_clear,
         musi_gl_color_buffer_bit, musi_glfw_key_escape,
         musi_glfw_key_up, musi_glfw_key_down, musi_glfw_key_left, musi_glfw_key_right,
         musi_glfw_key_w, musi_glfw_key_a, musi_glfw_key_s, musi_glfw_key_d
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
const KEY_UP := musi_glfw_key_up();
const KEY_DOWN := musi_glfw_key_down();
const KEY_LEFT := musi_glfw_key_left();
const KEY_RIGHT := musi_glfw_key_right();
const KEY_W := musi_glfw_key_w();
const KEY_A := musi_glfw_key_a();
const KEY_S := musi_glfw_key_s();
const KEY_D := musi_glfw_key_d();
const COLOR_BUFFER := musi_gl_color_buffer_bit();

writeln("Press arrow keys or WASD to change background color.");
writeln("Press ESC to exit.");

var r := 0.2;
var g := 0.2;

while musi_glfw_should_close(win) = 0 loop (
    const width := musi_glfw_get_width(win);
    const height := musi_glfw_get_height(win);

    // Change color based on key presses
    if musi_glfw_get_key(win, KEY_UP) /= 0 then r <- r + 0.01
    else if musi_glfw_get_key(win, KEY_W) /= 0 then r <- r + 0.01;

    if musi_glfw_get_key(win, KEY_DOWN) /= 0 then r <- r - 0.01
    else if musi_glfw_get_key(win, KEY_S) /= 0 then r <- r - 0.01;

    if musi_glfw_get_key(win, KEY_RIGHT) /= 0 then g <- g + 0.01
    else if musi_glfw_get_key(win, KEY_D) /= 0 then g <- g + 0.01;

    if musi_glfw_get_key(win, KEY_LEFT) /= 0 then g <- g - 0.01
    else if musi_glfw_get_key(win, KEY_A) /= 0 then g <- g - 0.01;

    // Clamp values
    if r < 0.0 then r <- 0.0;
    if r > 1.0 then r <- 1.0;
    if g < 0.0 then g <- 0.0;
    if g > 1.0 then g <- 1.0;

    musi_gl_viewport(width, height);
    musi_gl_clear_color(r, g);
    musi_gl_clear(COLOR_BUFFER);

    musi_glfw_swap(win);
    musi_glfw_poll_events();

    if musi_glfw_get_key(win, KEY_ESCAPE) /= 0 then (
        musi_glfw_destroy(win);
        musi_glfw_terminate();
    );
);

musi_glfw_destroy(win);
musi_glfw_terminate();
