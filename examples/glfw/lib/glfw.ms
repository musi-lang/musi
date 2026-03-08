// GLFW + OpenGL bindings for Musi
// Provides safe wrappers around FFI functions

// -- GLFW Core --
#[link("libmusi_glfw")] export extrin fn musi_glfw_init(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_terminate(): Unit;
#[link("libmusi_glfw")] export extrin fn musi_glfw_create_window(w: Int, h: Int): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_destroy(win: Int): Unit;
#[link("libmusi_glfw")] export extrin fn musi_glfw_make_current(win: Int): Unit;
#[link("libmusi_glfw")] export extrin fn musi_glfw_swap_interval(interval: Int): Unit;
#[link("libmusi_glfw")] export extrin fn musi_glfw_swap(win: Int): Unit;
#[link("libmusi_glfw")] export extrin fn musi_glfw_poll_events(): Unit;
#[link("libmusi_glfw")] export extrin fn musi_glfw_should_close(win: Int): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_get_time(): Float;
#[link("libmusi_glfw")] export extrin fn musi_glfw_get_key(win: Int, key: Int): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_get_cursor_x(win: Int): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_get_cursor_y(win: Int): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_get_width(win: Int): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_get_height(win: Int): Int;

// -- OpenGL --
#[link("libmusi_glfw")] export extrin fn musi_gl_viewport(w: Int, h: Int): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_clear_color(r: Float, g: Float): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_clear_black(): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_clear(mask: Int): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_setup_projection(aspect: Float): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_setup_ortho(w: Int, h: Int): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_setup_camera(): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_setup_cube_camera(): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_translate_z(z: Float): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_rotate_x(angle: Float): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_rotate_y(angle: Float): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_rotate_z(angle: Float): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_color3(r: Float, g: Float): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_color3_rgb(rgb: Int): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_enable_depth(): Unit;

// -- Shape Drawing --
#[link("libmusi_glfw")] export extrin fn musi_gl_draw_triangle(): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_draw_rect(x: Float, y: Float): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_draw_circle(cx: Float, cy: Float): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_draw_cube_wireframe(): Unit;
#[link("libmusi_glfw")] export extrin fn musi_gl_draw_cube_solid(): Unit;

// -- Constants --
#[link("libmusi_glfw")] export extrin fn musi_gl_color_buffer_bit(): Int;
#[link("libmusi_glfw")] export extrin fn musi_gl_depth_buffer_bit(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_escape(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_space(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_up(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_down(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_left(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_right(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_w(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_a(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_s(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_key_d(): Int;
