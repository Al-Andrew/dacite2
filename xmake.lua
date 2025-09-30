add_rules("mode.debug", "mode.release")

set_languages("cxx23")

target("dacite2")
    set_kind("binary")
    add_files("src/*.cpp")
    