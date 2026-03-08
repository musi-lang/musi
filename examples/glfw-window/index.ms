import { musi_glfw_init, musi_glfw_create_window, musi_glfw_poll_events,
         musi_glfw_should_close, musi_glfw_destroy, musi_glfw_terminate,
         musi_glfw_swap } from "./glfw";

const ok := musi_glfw_init();
if ok /= 1 then writeln("GLFW init failed");
const win := musi_glfw_create_window(800, 600);
while musi_glfw_should_close(win) = 0 loop (
    musi_glfw_poll_events();
    musi_glfw_swap(win);
);
musi_glfw_destroy(win);
musi_glfw_terminate();
