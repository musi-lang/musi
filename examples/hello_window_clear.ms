// https://learnopengl.com/code_viewer_gh.php?code=src/1.getting_started/1.2.hello_window_clear/hello_window_clear.cpp
val io := import "std/io.ms";

record GLFWwindow {}; // opaque

extern "C" unsafe {
	fn glfwInit(): Int32;
	fn glfwWindowHint(hint: Int32, value: Int32);
	fn glfwCreateWindow(width: Int32, height: Int32, title: ^Nat8, monitor: ^Any, share: ^Any): ?^GLFWwindow;
	fn glfwMakeContextCurrent(window: ^GLFWwindow);
	fn glfwSetFramebufferSizeCallback(window: ^GLFWwindow, cb: ^(^GLFWwindow, Int32, Int32) -> Unit);
	fn glfwWindowShouldClose(window: ^GLFWwindow): Int32;
	fn glfwSwapBuffers(window: ^GLFWwindow);
	fn glfwPollEvents();
	fn glfwTerminate();
	fn gladLoadGLLoader(proc: ^Any): Int32;
	fn glClearColor(r: Bin32, g: Bin32, b: Bin32, a: Bin32);
	fn glClear(mask: Int32);
	fn glViewport(x: Int32, y: Int32, w: Int32, h: Int32);
	fn glfwGetProcAddress(name: ^Nat8): ^Any;
	fn glfwSetWindowShouldClose(window: ^GLFWwindow, value: Int32);
	fn glfwGetKey(window: ^GLFWwindow, key: Int32): Int32;
};

val SCR_WIDTH: Int32 := 800;
val SCR_HEIGHT: Int32 := 600;
val GLFW_CONTEXT_VERSION_MAJOR: Int32 := 0x00022002; // from glfw3.h
val GLFW_CONTEXT_VERSION_MINOR: Int32 := 0x00022003;
val GLFW_OPENGL_PROFILE: Int32 := 0x00022008;
val GLFW_OPENGL_CORE_PROFILE: Int32 := 0x00032001;
val GLFW_OPENGL_FORWARD_COMPAT: Int32 := 0x00022006;
val GL_TRUE: Int32 := 1;
val GL_COLOR_BUFFER_BIT: Int32 := 0x00004000;
val GLFW_KEY_ESCAPE: Int32 := 256;
val GLFW_PRESS: Int32 := 1;

unsafe {
  // glfw: initialize and configure
  // ------------------------------
  if glfwInit() = 0 {
	  io#writeln("Failed to initialize GLFW");
	  break;
  };
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);

  // process all input: query GLFW whether relevant keys are pressed/released this frame and react accordingly
  // ---------------------------------------------------------------------------------------------------------
  val framebuffer_size_callback := fn (window: ^GLFWwindow, width: Int32, height: Int32) {
    // make sure the viewport matches the new window dimensions; note that width and
    // height will be significantly larger than specified on retina displays.
    glViewport(0, 0, width, height);
  };

  // glfw: whenever the window size changed (by OS or user resize) this callback function executes
  // ---------------------------------------------------------------------------------------------
  val process_input := fn (window: ^GLFWwindow) {
    if glfwGetKey(window, GLFW_KEY_ESCAPE) = GLFW_PRESS {
      glfwSetWindowShouldClose(window, 1);
    };
  };

  // glfw window creation
  // --------------------
  val window_opt := glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, "LearnOpenGL", 0, 0);
  match window_opt {
  case None => {
	  io#writeln("Failed to create GLFW window");
	  glfwTerminate();
	  break;
  },
  case Some(window) => {
	  glfwMakeContextCurrent(window);
	  glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

    // glad: load all OpenGL function pointers
    // ---------------------------------------
	  if gladLoadGLLoader(glfwGetProcAddress) = 0 {
		  io#writeln("Failed to initialize GLAD");
		  break;
	  };

    // render loop
    // -----------
	  while glfwWindowShouldClose(window) = 0 {
      // input
      // -----
	    process_input(window);

      // render
      // ------
	    glClearColor(0.2, 0.3, 0.3, 1.0);
	    glClear(GL_COLOR_BUFFER_BIT);

      // glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
      // -------------------------------------------------------------------------------
	    glfwSwapBuffers(window);
	    glfwPollEvents();
	  };

    // glfw: terminate, clearing all previously allocated GLFW resources.
    // ------------------------------------------------------------------
	  glfwTerminate();
  }
  };
};
