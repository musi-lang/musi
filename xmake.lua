set_project("musi")
set_version("0.1.0")
set_description("The Musi Programming Language")

add_rules("mode.debug", "mode.release")
add_rules("plugin.compile_commands.autoupdate", {outputdir = "."})
add_rules("mode.coverage")

add_requires("doctest")

target("musi")
    set_kind("binary")
    if is_plat("windows") then
        set_toolchains("clang-cl")

        add_cxxflags("-std=c++23")
    else
        set_languages("cxx23")
        set_toolchains("clang")
    end

    add_files("runtime/src/*.cpp")
    add_includedirs("inc", "runtime/src")

    if is_mode("debug") then
        set_warnings("everything")
        set_optimize("none")

        add_cxxflags("-Wno-switch-default", "-Wno-switch-enum")
        add_cxxflags("-Wno-c++98-compat", "-Wno-c++98-compat-pedantic")

        if is_plat("windows") then
            add_cxxflags("/Zi", "/Od", "/Oy-", "/GS")
            add_cxxflags("/MD")  -- Release runtime for AddressSanitizer
            -- add_cxxflags("/fsanitize=address")
            -- add_ldflags("/fsanitize=address")
        else
            add_cxxflags("-fsafe-buffer-usage-suggestions")

            add_cxxflags("-g3", "-O0", "-glldb")
            add_cxxflags("-fno-omit-frame-pointer", "-fno-optimize-sibling-calls", "-fstack-protector-strong")
            add_cxxflags("-fsanitize=address,undefined")
            add_ldflags("-fsanitize=address,undefined", {force = true})

            -- VALGRIND: disable ASAN when running under valgrind
            -- RUN WITH: valgrind --leak-check=full --track-origins=yes ./build/linux/x86_64/debug/musi
        end
    else
        set_optimize("smallest")
        set_strip("all")

        add_cxxflags("-fstack-protector-strong", "-D_FORTIFY_SOURCE=2")
    end

target("musi_tests")
    set_kind("binary")
    if is_plat("windows") then
        set_toolchains("clang-cl")
        add_cxxflags("-std=c++23")
    else
        set_languages("cxx23")
        set_toolchains("clang")
    end

    add_files("runtime/test/*.cpp")
    add_files(
        "runtime/src/loader.cpp",
        "runtime/src/header.cpp",
        "runtime/src/vm.cpp",
        "runtime/src/object.cpp",
        "runtime/src/intrinsics.cpp"
    )
    add_includedirs("inc", "runtime/src")
    add_packages("doctest")

    if is_mode("debug") then
        set_warnings("everything")
        set_optimize("none")
        add_cxxflags("-Wno-switch-default", "-Wno-switch-enum")
        add_cxxflags("-Wno-c++98-compat", "-Wno-c++98-compat-pedantic")
        if not is_plat("windows") then
            add_cxxflags("-g3", "-O0")
        end
    end
