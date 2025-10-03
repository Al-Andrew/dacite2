#pragma once

#include "codegen.hpp"
#include <vector>
#include <cstdint>

namespace dacite {

// Virtual Machine for executing compiled bytecode
class VM {
public:
    // Load a compiled module into the VM
    bool load_module(const CompiledModule& module);
    
    // Execute the loaded modules
    bool run();
    
    // Get the current stack (for debugging)
    const std::vector<uint64_t>& get_stack() const { return stack; }
    
    // Clear all loaded modules and reset the VM state
    void reset();

private:
    std::vector<CompiledModule> modules;
    std::vector<uint64_t> stack;
    
    // x86-style stack management
    size_t rsp = 0;  // Stack pointer (index into stack)
    size_t rbp = 0;  // Base pointer (frame pointer)
};

} // namespace dacite