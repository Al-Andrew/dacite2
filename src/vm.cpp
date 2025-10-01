#include "vm.hpp"
#include <cstdio>

namespace dacite {

bool VM::load_module(const CompiledModule& module) {
    if(!module.is_valid()) {
        fprintf(stderr, "Error: Invalid module\n");
        return false;
    }
    modules.push_back(module);
    return true;
}

bool VM::run() {
    uint32_t current_module_index = 0;
    if(modules.size() == 0) {
        fprintf(stderr, "Error: No module loaded\n");
        return false;
    }

    const CompiledModule& module = modules[current_module_index];
    size_t pc = 0; // program counter
    while(pc < module.bytecode.size()) {
        dacite::BytecodeOp op = static_cast<dacite::BytecodeOp>(module.bytecode[pc]);
        switch(op) {
            case dacite::BytecodeOp::PRINT: {
                if(stack.size() < 1) {
                    fprintf(stderr, "Error: PRINT instruction requires at least 1 value on the stack\n");
                    return false;
                }
                uint64_t value = stack.back();
                stack.pop_back();
                printf("%llu\n", value);
                pc += 1;
            } break;
            case dacite::BytecodeOp::ADD: {
                if(stack.size() < 2) {
                    fprintf(stderr, "Error: ADD instruction requires at least 2 values on the stack\n");
                    return false;
                }
                uint64_t b = stack.back(); stack.pop_back();
                uint64_t a = stack.back(); stack.pop_back();
                stack.push_back(a + b);
                pc += 1;
            } break;
            case dacite::BytecodeOp::SUBTRACT: {
                if(stack.size() < 2) {
                    fprintf(stderr, "Error: SUBTRACT instruction requires at least 2 values on the stack\n");
                    return false;
                }
                uint64_t b = stack.back(); stack.pop_back();
                uint64_t a = stack.back(); stack.pop_back();
                stack.push_back(a - b);
                pc += 1;
            } break;
            case dacite::BytecodeOp::MULTIPLY: {
                if(stack.size() < 2) {
                    fprintf(stderr, "Error: MULTIPLY instruction requires at least 2 values on the stack\n");
                    return false;
                }
                uint64_t b = stack.back(); stack.pop_back();
                uint64_t a = stack.back(); stack.pop_back();
                stack.push_back(a * b);
                pc += 1;
            } break;
            case dacite::BytecodeOp::DIVIDE: {
                if(stack.size() < 2) {
                    fprintf(stderr, "Error: DIVIDE instruction requires at least 2 values on the stack\n");
                    return false;
                }
                uint64_t b = stack.back(); stack.pop_back();
                if(b == 0) {
                    fprintf(stderr, "Error: Division by zero\n");
                    return false;
                }
                uint64_t a = stack.back(); stack.pop_back();
                stack.push_back(a / b);
                pc += 1;
            } break;
            case dacite::BytecodeOp::PUSH_CONST: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: PUSH_CONST instruction missing operand\n");
                    return false;
                }
                uint32_t const_index = module.bytecode[pc + 1];
                if(const_index >= module.constants.size()) {
                    fprintf(stderr, "Error: PUSH_CONST instruction has invalid constant index %u\n", const_index);
                    return false;
                }
                uint64_t value = module.constants[const_index];
                stack.push_back(value);
                pc += 2; // advance past PUSH_CONST and its operand
            } break;
            case dacite::BytecodeOp::POP: {
                if(stack.size() < 1) {
                    fprintf(stderr, "Error: POP instruction requires at least 1 value on the stack\n");
                    return false;
                }
                stack.pop_back();
                pc += 1;
            } break;
            case dacite::BytecodeOp::RESERVE: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: RESERVE instruction missing operand\n");
                    return false;
                }
                uint32_t how_much = module.bytecode[pc + 1];
                for(uint32_t i = 0; i < how_much / sizeof(uint64_t); i++) {
                    stack.push_back(0); // initialize reserved space with zeros
                }
                pc += 2; // advance past RESERVE and its operand
            } break;
            case dacite::BytecodeOp::STORE: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: STORE instruction missing operand\n");
                    return false;
                }
                uint32_t where = module.bytecode[pc + 1];
                if(stack.size() < 1) {
                    fprintf(stderr, "Error: STORE instruction requires at least 1 value on the stack\n");
                    return false;
                }
                if(where / sizeof(uint64_t) >= stack.size()) {
                    fprintf(stderr, "Error: STORE instruction has invalid stack offset %u\n", where);
                    return false;
                }
                uint64_t value = stack.back();
                stack.pop_back();
                stack[where / sizeof(uint64_t)] = value;
                pc += 2; // advance past STORE and its operand
            } break;
            case dacite::BytecodeOp::LOAD: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: LOAD instruction missing operand\n");
                    return false;
                }
                uint32_t where = module.bytecode[pc + 1];
                if(where / sizeof(uint64_t) >= stack.size()) {
                    fprintf(stderr, "Error: LOAD instruction has invalid stack offset %u\n", where);
                    return false;
                }
                uint64_t value = stack[where / sizeof(uint64_t)];
                stack.push_back(value);
                pc += 2; // advance past LOAD and its operand
            } break;
            default: {
                fprintf(stderr, "Error: Unknown bytecode operation %u at pc %zu\n", (uint32_t)op, pc);
                return false;
            } break;
        }
    }
    return true;
}

void VM::reset() {
    modules.clear();
    stack.clear();
}

} // namespace dacite