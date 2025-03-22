set_project("musi")
set_version("0.1.0")
set_description("The Musi Programming Language")

add_rules("mode.debug", "mode.release")

add_requires("magic_enum")
add_requires("nlohmann_json")
add_requires("spdlog")

target("musi")
    set_kind("binary")
    set_languages("cxx23")
    set_toolchains("clang")
    
    add_files("src/*.cpp")
    add_includedirs("include", "src")

    add_packages("magic_enum")
    add_packages("nlohmann_json")
    add_packages("spdlog")

    if is_mode("debug") then
        set_warnings("everything")
        set_optimize("none")
        
        add_cxxflags("-O1")
        add_cxxflags("-g3")
        add_cxxflags("-glldb")
        add_cxxflags("-Wno-switch-default")
        add_cxxflags("-Wno-switch-enum")
        add_cxxflags("-Wno-c++98-compat", "-Wno-c++98-compat-pedantic")
        
        add_cxxflags("-fsanitize=address,undefined")
        add_cxxflags("-fno-omit-frame-pointer")
        add_cxxflags("-fno-optimize-sibling-calls")
        add_cxxflags("-fstack-protector-strong")
        
        add_ldflags("-fsanitize=address,undefined", {force = true})
    else
        set_optimize("smallest")
        set_strip("all")
        
        add_cxxflags("-fstack-protector-strong")
        add_cxxflags("-D_FORTIFY_SOURCE=2")
    end