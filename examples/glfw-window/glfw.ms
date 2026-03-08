#[link("libmusi_glfw")] export extrin fn musi_glfw_init(): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_create_window(w: Int, h: Int): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_poll_events(): Unit;
#[link("libmusi_glfw")] export extrin fn musi_glfw_should_close(win: Int): Int;
#[link("libmusi_glfw")] export extrin fn musi_glfw_destroy(win: Int): Unit;
#[link("libmusi_glfw")] export extrin fn musi_glfw_terminate(): Unit;
#[link("libmusi_glfw")] export extrin fn musi_glfw_swap(win: Int): Unit;
