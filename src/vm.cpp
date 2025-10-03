#include "vm.hpp"
#include <cstdint>
#include <cstdio>
#include <vector>
#include <cinttypes>


#define DACITE_TRACE_EXECUTION 1

#if DACITE_TRACE_EXECUTION
    #define DBG_PRINT(X, ...) printf(X "\n", ##__VA_ARGS__)
#else
    #define DBG_PRINT(X, ...)
#endif

namespace dacite {

// Helper function to convert BytecodeOp to string for tracing
const char* bytecode_op_to_string(BytecodeOp op) {
    switch(op) {
        case BytecodeOp::PRINT: return "PRINT";
        case BytecodeOp::ADD: return "ADD";
        case BytecodeOp::SUBTRACT: return "SUBTRACT";
        case BytecodeOp::MULTIPLY: return "MULTIPLY";
        case BytecodeOp::DIVIDE: return "DIVIDE";
        case BytecodeOp::PUSH_CONST: return "PUSH_CONST";
        case BytecodeOp::POP: return "POP";
        case BytecodeOp::RESERVE: return "RESERVE";
        case BytecodeOp::STORE: return "STORE";
        case BytecodeOp::LOAD: return "LOAD";
        case BytecodeOp::JMP: return "JMP";
        case BytecodeOp::CALL: return "CALL";
        case BytecodeOp::RETURN: return "RETURN";
        case BytecodeOp::HALT: return "HALT";
        default: return "UNKNOWN";
    }
}

// Helper function to print the current stack state
void print_stack_state(const std::vector<uint64_t>& stack) {
    printf("    Stack [%zu]: [", stack.size());
    for(size_t i = 0; i < stack.size(); ++i) {
        if(i > 0) printf(", ");
        printf("%" PRIu64, stack[i]);
    }
    printf("]\n");
}

#if DACITE_TRACE_EXECUTION
    #define PRINT_STACK_STATE() print_stack_state(stack)
    #define TRACE_INSTRUCTION(op) DBG_PRINT("PC %zu: Executing %s", pc, bytecode_op_to_string(op)); PRINT_STACK_STATE()
    #define TRACE_INSTRUCTION_WITH_OPERAND(op, operand) DBG_PRINT("PC %zu: Executing %s (operand: %d)", pc, bytecode_op_to_string(op), operand); PRINT_STACK_STATE()
#else
    #define PRINT_STACK_STATE()
    #define TRACE_INSTRUCTION(op)
    #define TRACE_INSTRUCTION_WITH_OPERAND(op, operand)
#endif

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
                TRACE_INSTRUCTION(op);
                if(stack.size() < 1) {
                    fprintf(stderr, "Error: PRINT instruction requires at least 1 value on the stack\n");
                    return false;
                }
                uint64_t value = stack.back();
                stack.pop_back();
                printf("%" PRIu64 "\n", value);
                pc += 1;
            } break;
            case dacite::BytecodeOp::ADD: {
                TRACE_INSTRUCTION(op);
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
                TRACE_INSTRUCTION(op);
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
                TRACE_INSTRUCTION(op);
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
                TRACE_INSTRUCTION(op);
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
                TRACE_INSTRUCTION_WITH_OPERAND(op, value);
                stack.push_back(value);
                pc += 2; // advance past PUSH_CONST and its operand
            } break;
            case dacite::BytecodeOp::POP: {
                TRACE_INSTRUCTION(op);
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
                TRACE_INSTRUCTION_WITH_OPERAND(op, how_much);
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
                TRACE_INSTRUCTION_WITH_OPERAND(op, where);
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
                stack[last_call_stack_size + where / sizeof(uint64_t)] = value;
                pc += 2; // advance past STORE and its operand
            } break;
            case dacite::BytecodeOp::LOAD: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: LOAD instruction missing operand\n");
                    return false;
                }
                int32_t where = module.bytecode[pc + 1];
                TRACE_INSTRUCTION_WITH_OPERAND(op, where);
                // if(where / sizeof(uint64_t) >= stack.size()) {
                //     fprintf(stderr, "Error: LOAD instruction has invalid stack offset %u\n", where);
                //     return false;
                // }
                uint32_t computed_index = (int32_t)last_call_stack_size + where / sizeof(uint64_t);
                DBG_PRINT("last_call_stack_size: %" PRIu64 ", where: %d, computed index: %d", last_call_stack_size, where, computed_index);
                uint64_t value = stack[computed_index];
                stack.push_back(value);
                pc += 2; // advance past LOAD and its operand
            } break;
            case dacite::BytecodeOp::HALT: {
                TRACE_INSTRUCTION(op);
                return true; // stop execution
            } break;
            case dacite::BytecodeOp::JMP: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: JMP instruction missing operand\n");
                    return false;
                }
                uint32_t where = module.bytecode[pc + 1];
                TRACE_INSTRUCTION_WITH_OPERAND(op, where);
                if(where >= module.bytecode.size()) {
                    fprintf(stderr, "Error: JMP instruction has invalid target %u\n", where);
                    return false;
                }
                pc = where; // jump to target
            } break;
            case dacite::BytecodeOp::RETURN: {
                TRACE_INSTRUCTION(op);
                // the stack right now should look like this:
                // [... previous stack ... | (return_address) previous_last_call_stack | ... function stack ... ] return_value
                // we need to pop the return_value
                // we need to pop everything above previous_last_call_stack
                if(stack.size() < 1) {
                    fprintf(stderr, "Error: RETURN instruction requires at least 1 value on the stack\n");
                    return false;
                }
                if(last_call_stack_size == 0 || last_call_stack_size > stack.size()) {
                    fprintf(stderr, "Error: RETURN instruction has invalid last_call_stack_size %" PRIu64 "\n", last_call_stack_size);
                    return false;
                }
                uint64_t return_value = stack.back();
                stack.pop_back();

                // pop everything above last_call_stack_size
                while(stack.size() > last_call_stack_size) {
                    stack.pop_back();
                }
                // now the top of the stack should be the previous last_call_stack_size
                uint64_t previous_last_call_stack_size = stack.back();
                stack.pop_back();
                last_call_stack_size = previous_last_call_stack_size;
                // set pc to the return address which is now on top of the stack
                if(stack.size() < 1) {
                    fprintf(stderr, "Error: RETURN instruction requires a return address on the stack\n");
                    return false;
                }
                uint64_t return_address = stack.back();
                stack.pop_back();
                if(return_address >= module.bytecode.size()) {
                    fprintf(stderr, "Error: RETURN instruction has invalid return address %" PRIu64 "\n", return_address);
                    return false;
                }
                DBG_PRINT("Returning to address %" PRIu64 "\n", return_address);
                pc = return_address;

                // push the return value back onto the stack
                stack.push_back(return_value);
            } break;
            case dacite::BytecodeOp::CALL: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: CALL instruction missing operand\n");
                    return false;
                }
                uint32_t function_offset = module.bytecode[pc + 1];
                TRACE_INSTRUCTION_WITH_OPERAND(op, function_offset);
                if(function_offset >= module.bytecode.size()) {
                    fprintf(stderr, "Error: CALL instruction has invalid function offset %u\n", function_offset);
                    return false;
                }

                // save the return adress
                stack.push_back(pc + 2); // return address is after the CALL instruction and its operand
                // save the previous last_call_stack_size
                stack.push_back(last_call_stack_size);
                // Save current pc to return to after function call
                last_call_stack_size = stack.size();
                pc = function_offset; // jump to function
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