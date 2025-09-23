add_rules("mode.debug", "mode.release")

target("dacite2")
    set_kind("binary")
    add_files("src/*.cpp")
    