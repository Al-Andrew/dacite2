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
        case BytecodeOp::PUSH_REG: return "PUSH_REG";
        case BytecodeOp::POP_REG: return "POP_REG";
        case BytecodeOp::LOAD_REG: return "LOAD_REG";
        case BytecodeOp::STORE_REG: return "STORE_REG";
        case BytecodeOp::HALT: return "HALT";
        default: return "UNKNOWN";
    }
}

// Helper function to print the current stack state
void print_stack_state(const std::vector<uint64_t>& stack, const std::vector<size_t>& registers) {
    size_t rsp = registers[static_cast<size_t>(dacite::RegisterId::RSP)];
    size_t rbp = registers[static_cast<size_t>(dacite::RegisterId::RBP)];
    printf("    Stack [size=%zu, RSP=%zu, RBP=%zu]: [", stack.size(), rsp, rbp);
    for(size_t i = 0; i < rsp && i < stack.size(); ++i) {
        if(i > 0) printf(", ");
        printf("%" PRIu64, stack[i]);
        if(i == rbp) printf("(RBP)");
    }
    printf("]\n");
}

#if DACITE_TRACE_EXECUTION
    #define PRINT_STACK_STATE() print_stack_state(stack, registers)
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
    
    // Initialize registers
    registers[static_cast<size_t>(dacite::RegisterId::RSP)] = 0;  // RSP
    registers[static_cast<size_t>(dacite::RegisterId::RBP)] = 0;  // RBP
    
    // Helper references for cleaner code
    auto& rsp = registers[static_cast<size_t>(dacite::RegisterId::RSP)];
    auto& rbp = registers[static_cast<size_t>(dacite::RegisterId::RBP)];
    
    while(pc < module.bytecode.size()) {
        dacite::BytecodeOp op = static_cast<dacite::BytecodeOp>(module.bytecode[pc]);
        
        switch(op) {
            case dacite::BytecodeOp::PRINT: {
                TRACE_INSTRUCTION(op);
                if(rsp == 0) {
                    fprintf(stderr, "Error: PRINT instruction requires at least 1 value on the stack\n");
                    return false;
                }
                uint64_t value = stack[rsp - 1];
                rsp--;
                printf("%" PRIu64 "\n", value);
                pc += 1;
            } break;
            case dacite::BytecodeOp::ADD: {
                TRACE_INSTRUCTION(op);
                if(rsp < 2) {
                    fprintf(stderr, "Error: ADD instruction requires at least 2 values on the stack\n");
                    return false;
                }
                uint64_t b = stack[rsp - 1];
                uint64_t a = stack[rsp - 2];
                rsp -= 2;
                stack[rsp] = a + b;
                rsp++;
                pc += 1;
            } break;
            case dacite::BytecodeOp::SUBTRACT: {
                TRACE_INSTRUCTION(op);
                if(rsp < 2) {
                    fprintf(stderr, "Error: SUBTRACT instruction requires at least 2 values on the stack\n");
                    return false;
                }
                uint64_t b = stack[rsp - 1];
                uint64_t a = stack[rsp - 2];
                rsp -= 2;
                stack[rsp] = a - b;
                rsp++;
                pc += 1;
            } break;
            case dacite::BytecodeOp::MULTIPLY: {
                TRACE_INSTRUCTION(op);
                if(rsp < 2) {
                    fprintf(stderr, "Error: MULTIPLY instruction requires at least 2 values on the stack\n");
                    return false;
                }
                uint64_t b = stack[rsp - 1];
                uint64_t a = stack[rsp - 2];
                rsp -= 2;
                stack[rsp] = a * b;
                rsp++;
                pc += 1;
            } break;
            case dacite::BytecodeOp::DIVIDE: {
                TRACE_INSTRUCTION(op);
                if(rsp < 2) {
                    fprintf(stderr, "Error: DIVIDE instruction requires at least 2 values on the stack\n");
                    return false;
                }
                uint64_t b = stack[rsp - 1];
                if(b == 0) {
                    fprintf(stderr, "Error: Division by zero\n");
                    return false;
                }
                uint64_t a = stack[rsp - 2];
                rsp -= 2;
                stack[rsp] = a / b;
                rsp++;
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
                TRACE_INSTRUCTION_WITH_OPERAND(op, const_index);
                
                // Ensure stack has enough space
                if(rsp >= stack.size()) {
                    stack.resize(rsp + 1);
                }
                stack[rsp] = value;
                rsp++;
                pc += 2; // advance past PUSH_CONST and its operand
            } break;
            case dacite::BytecodeOp::POP: {
                TRACE_INSTRUCTION(op);
                if(rsp == 0) {
                    fprintf(stderr, "Error: POP instruction requires at least 1 value on the stack\n");
                    return false;
                }
                rsp--;
                pc += 1;
            } break;
            case dacite::BytecodeOp::RESERVE: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: RESERVE instruction missing operand\n");
                    return false;
                }
                uint32_t how_much = module.bytecode[pc + 1];
                TRACE_INSTRUCTION_WITH_OPERAND(op, how_much);
                
                uint32_t slots = how_much / sizeof(uint64_t);
                // Ensure stack has enough space
                if(rsp + slots > stack.size()) {
                    stack.resize(rsp + slots);
                }
                // Initialize reserved space with zeros
                for(uint32_t i = 0; i < slots; i++) {
                    stack[rsp + i] = 0;
                }
                rsp += slots;
                pc += 2; // advance past RESERVE and its operand
            } break;
            case dacite::BytecodeOp::STORE: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: STORE instruction missing operand\n");
                    return false;
                }
                uint32_t where = module.bytecode[pc + 1];
                TRACE_INSTRUCTION_WITH_OPERAND(op, where);
                if(rsp == 0) {
                    fprintf(stderr, "Error: STORE instruction requires at least 1 value on the stack\n");
                    return false;
                }
                uint32_t stack_offset = rbp + where / sizeof(uint64_t);
                if(stack_offset >= stack.size()) {
                    fprintf(stderr, "Error: STORE instruction has invalid stack offset %u\n", stack_offset);
                    return false;
                }
                uint64_t value = stack[rsp - 1];
                rsp--;
                stack[stack_offset] = value;
                pc += 2; // advance past STORE and its operand
            } break;
            case dacite::BytecodeOp::LOAD: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: LOAD instruction missing operand\n");
                    return false;
                }
                int32_t where = module.bytecode[pc + 1];
                TRACE_INSTRUCTION_WITH_OPERAND(op, where);
                
                uint32_t stack_offset = rbp + where / sizeof(uint64_t);
                if(stack_offset >= stack.size()) {
                    fprintf(stderr, "Error: LOAD instruction has invalid stack offset %u\n", stack_offset);
                    return false;
                }
                DBG_PRINT("rbp: %zu, where: %d, stack_offset: %u", rbp, where, stack_offset);
                uint64_t value = stack[stack_offset];
                
                // Ensure stack has enough space
                if(rsp >= stack.size()) {
                    stack.resize(rsp + 1);
                }
                stack[rsp] = value;
                rsp++;
                pc += 2; // advance past LOAD and its operand
            } break;
            case dacite::BytecodeOp::PUSH_REG: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: PUSH_REG instruction missing operand\n");
                    return false;
                }
                uint32_t reg_id = module.bytecode[pc + 1];
                TRACE_INSTRUCTION_WITH_OPERAND(op, reg_id);
                
                if(reg_id >= registers.size()) {
                    fprintf(stderr, "Error: PUSH_REG invalid register id %u\n", reg_id);
                    return false;
                }
                
                // Ensure stack has enough space
                if(rsp >= stack.size()) {
                    stack.resize(rsp + 1);
                }
                stack[rsp] = registers[reg_id];
                rsp++;
                pc += 2;
            } break;
            case dacite::BytecodeOp::POP_REG: {
                if(pc + 1 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: POP_REG instruction missing operand\n");
                    return false;
                }
                uint32_t reg_id = module.bytecode[pc + 1];
                TRACE_INSTRUCTION_WITH_OPERAND(op, reg_id);
                
                if(reg_id >= registers.size()) {
                    fprintf(stderr, "Error: POP_REG invalid register id %u\n", reg_id);
                    return false;
                }
                
                if(rsp == 0) {
                    fprintf(stderr, "Error: POP_REG instruction requires at least 1 value on the stack\n");
                    return false;
                }
                rsp--;
                registers[reg_id] = stack[rsp];
                pc += 2;
            } break;



            case dacite::BytecodeOp::LOAD_REG: {
                if(pc + 2 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: LOAD_REG instruction missing operands\n");
                    return false;
                }
                uint32_t reg_id = module.bytecode[pc + 1];
                int32_t offset = (int32_t)module.bytecode[pc + 2];
                TRACE_INSTRUCTION_WITH_OPERAND(op, reg_id);
                
                if(reg_id >= registers.size()) {
                    fprintf(stderr, "Error: LOAD_REG invalid register id %u\n", reg_id);
                    return false;
                }
                
                // Calculate address: REG + (offset / 8) where offset can be negative for parameters
                int64_t addr = (int64_t)registers[reg_id] + (offset / (int32_t)sizeof(uint64_t));
                if(addr < 0 || addr >= (int64_t)stack.size()) {
                    fprintf(stderr, "Error: LOAD_REG has invalid address %ld (reg[%u]=%zu, offset=%d)\n", addr, reg_id, registers[reg_id], offset);
                    return false;
                }
                
                // Ensure stack has enough space
                if(rsp >= stack.size()) {
                    stack.resize(rsp + 1);
                }
                stack[rsp] = stack[addr];
                rsp++;
                pc += 3;
            } break;
            case dacite::BytecodeOp::STORE_REG: {
                if(pc + 2 >= module.bytecode.size()) {
                    fprintf(stderr, "Error: STORE_REG instruction missing operands\n");
                    return false;
                }
                uint32_t reg_id = module.bytecode[pc + 1];
                int32_t offset = (int32_t)module.bytecode[pc + 2];
                TRACE_INSTRUCTION_WITH_OPERAND(op, reg_id);
                
                if(reg_id >= registers.size()) {
                    fprintf(stderr, "Error: STORE_REG invalid register id %u\n", reg_id);
                    return false;
                }
                
                if(rsp == 0) {
                    fprintf(stderr, "Error: STORE_REG requires at least 1 value on the stack\n");
                    return false;
                }
                
                // Calculate address: REG + (offset / 8) where offset can be negative for parameters
                int64_t addr = (int64_t)registers[reg_id] + (offset / (int32_t)sizeof(uint64_t));
                if(addr < 0 || addr >= (int64_t)stack.size()) {
                    fprintf(stderr, "Error: STORE_REG has invalid address %ld (reg[%u]=%zu, offset=%d)\n", addr, reg_id, registers[reg_id], offset);
                    return false;
                }
                
                rsp--;
                stack[addr] = stack[rsp];
                pc += 3;
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
                // In x86-style calling convention:
                // Stack layout: [...] [return_value] [old_rbp] [return_address] <- RSP
                // We need to:
                // 1. Get the return value (already on top)
                // 2. Restore RSP to point to old_rbp
                // 3. Restore RBP from stack
                // 4. Get return address
                // 5. Jump to return address
                // 6. Push return value back
                
                if(rsp == 0) {
                    fprintf(stderr, "Error: RETURN instruction requires at least 1 value on the stack\n");
                    return false;
                }
                
                // Get return value
                uint64_t return_value = stack[rsp - 1];
                
                // Restore RSP to the position where old RBP and return address are stored
                rsp = rbp;
                
                // Restore RBP
                if(rsp == 0) {
                    fprintf(stderr, "Error: RETURN instruction invalid stack state for RBP restore\n");
                    return false;
                }
                rsp--;
                rbp = stack[rsp];
                
                // Get return address
                if(rsp == 0) {
                    fprintf(stderr, "Error: RETURN instruction invalid stack state for return address\n");
                    return false;
                }
                rsp--;
                uint64_t return_address = stack[rsp];
                
                if(return_address >= module.bytecode.size()) {
                    fprintf(stderr, "Error: RETURN instruction has invalid return address %" PRIu64 "\n", return_address);
                    return false;
                }
                
                DBG_PRINT("Returning to address %" PRIu64 "\n", return_address);
                pc = return_address;

                // Push the return value back onto the stack
                if(rsp >= stack.size()) {
                    stack.resize(rsp + 1);
                }
                stack[rsp] = return_value;
                rsp++;
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

                // x86-style function call:
                // 1. Push return address
                // 2. Push old RBP
                // 3. Set RBP to current RSP
                // 4. Jump to function
                
                // Push return address (instruction after CALL)
                if(rsp >= stack.size()) {
                    stack.resize(rsp + 1);
                }
                stack[rsp] = pc + 2;
                rsp++;
                
                // Push old RBP
                if(rsp >= stack.size()) {
                    stack.resize(rsp + 1);
                }
                stack[rsp] = rbp;
                rsp++;
                
                // Set RBP to current RSP (establish new frame)
                rbp = rsp;
                
                // Jump to function
                pc = function_offset;
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
    registers[static_cast<size_t>(dacite::RegisterId::RSP)] = 0;
    registers[static_cast<size_t>(dacite::RegisterId::RBP)] = 0;
}

} // namespace dacite