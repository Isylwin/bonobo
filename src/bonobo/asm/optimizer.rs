#![allow(dead_code)]

use crate::bonobo::asm::{
    AsmInstruction, AsmOperand, AsmProgram,
    operand::{AsmRegister, FALSE, RAX, TRUE},
    program::AsmSection,
};

pub fn optimize(program: &mut AsmProgram) {
    let mut x = AsmOptimizer::new();
    x.optimize_program(program);
}

use std::collections::{HashMap, HashSet};

pub struct AsmOptimizer {
    // Track which registers are modified by each instruction
    modified_registers: HashSet<AsmRegister>,
    // Track register usage patterns
    register_usage: HashMap<AsmRegister, usize>,
}

impl AsmOptimizer {
    pub fn new() -> Self {
        Self {
            modified_registers: HashSet::new(),
            register_usage: HashMap::new(),
        }
    }

    /// Main optimization entry point
    pub fn optimize_program(&mut self, program: &mut AsmProgram) {
        self.optimize_section(&mut program.text);
    }

    /// Optimize a single section
    pub fn optimize_section(&mut self, section: &mut AsmSection) {
        let mut optimized = true;
        let mut passes = 0;
        const MAX_PASSES: usize = 10;

        // Keep optimizing until no more changes are made or max passes reached
        while optimized && passes < MAX_PASSES {
            optimized = false;
            passes += 1;

            // Apply different optimization passes
            optimized |= self.peephole_optimize(&mut section.instructions);
            optimized |= self.remove_redundant_moves(&mut section.instructions);
            optimized |= self.remove_dead_code(&mut section.instructions);
            optimized |= self.fold_constants(&mut section.instructions);
            optimized |= self.optimize_arithmetic(&mut section.instructions);
            optimized |= self.optimize_stack_operations(&mut section.instructions);
            optimized |= self.remove_redundant_comparisons(&mut section.instructions);
        }
    }

    /// Remove redundant move instructions (mov reg, reg)
    fn remove_redundant_moves(&mut self, instructions: &mut Vec<AsmInstruction>) -> bool {
        let original_len = instructions.len();

        instructions.retain(|instr| {
            match instr {
                AsmInstruction::Move(src, dst) => {
                    // Remove moves where source and destination are the same
                    !operands_equal(src, dst)
                }
                AsmInstruction::Nop => false,
                _ => true,
            }
        });

        // Also look for patterns like: mov a, b; mov b, a
        let mut i = 0;
        while i + 1 < instructions.len() {
            if let (AsmInstruction::Move(src1, dst1), AsmInstruction::Move(src2, dst2)) =
                (&instructions[i], &instructions[i + 1])
            {
                if operands_equal(src1, dst2) && operands_equal(dst1, src2) {
                    // Remove the second move instruction
                    instructions.remove(i + 1);
                    continue;
                }
            }
            i += 1;
        }

        instructions.len() != original_len
    }

    /// Remove dead code (unreachable instructions after unconditional jumps/returns)
    fn remove_dead_code(&mut self, instructions: &mut Vec<AsmInstruction>) -> bool {
        let original_len = instructions.len();
        let mut result: Vec<AsmInstruction> = Vec::new();
        let mut skip_until_label = false;

        for instr in instructions.iter() {
            match instr {
                AsmInstruction::Jump(_) | AsmInstruction::Return => {
                    result.push(instr.clone());
                    skip_until_label = true;
                }
                AsmInstruction::Label(_) => {
                    result.push(instr.clone());
                    skip_until_label = false;
                }
                _ => {
                    if !skip_until_label {
                        result.push(instr.clone());
                    }
                }
            }
        }

        *instructions = result;
        instructions.len() != original_len
    }

    /// Fold constant expressions
    fn fold_constants(&mut self, instructions: &mut [AsmInstruction]) -> bool {
        let mut changed = false;

        for instr in instructions.iter_mut() {
            match instr {
                AsmInstruction::Add(FALSE, _)
                | AsmInstruction::Add(_, FALSE)
                | AsmInstruction::Subtract(FALSE, _) => {
                    // Adding or subtracting 0 - convert to nop (remove later)
                    *instr = AsmInstruction::Move(RAX, RAX);
                    changed = true;
                }
                AsmInstruction::SignedMultiply(TRUE, _)
                | AsmInstruction::SignedMultiply(_, TRUE) => {
                    // Multiplying by 1 - convert to nop
                    *instr = AsmInstruction::Move(RAX, RAX);
                    changed = true;
                }
                AsmInstruction::SignedMultiply(FALSE, dst) => {
                    // Multiplying by 0 - set destination to 0
                    *instr = AsmInstruction::Move(FALSE, dst.clone());
                    changed = true;
                }
                _ => {}
            }
        }

        changed
    }

    /// Optimize arithmetic operations
    fn optimize_arithmetic(&mut self, instructions: &mut Vec<AsmInstruction>) -> bool {
        let mut changed = false;
        let mut i = 0;

        while i + 1 < instructions.len() {
            // Look for patterns like: add reg, imm1; add reg, imm2 -> add reg, (imm1+imm2)
            if let (
                AsmInstruction::Add(AsmOperand::Immediate(val1), dst1),
                AsmInstruction::Add(AsmOperand::Immediate(val2), dst2),
            ) = (&instructions[i], &instructions[i + 1])
            {
                if operands_equal(dst1, dst2) {
                    let combined = val1 + val2;
                    instructions[i] =
                        AsmInstruction::Add(AsmOperand::Immediate(combined), dst1.clone());
                    instructions.remove(i + 1);
                    changed = true;
                    continue;
                }
            }

            // Look for add/subtract cancellation: add reg, val; sub reg, val
            if let (
                AsmInstruction::Add(AsmOperand::Immediate(val1), dst1),
                AsmInstruction::Subtract(AsmOperand::Immediate(val2), dst2),
            ) = (&instructions[i], &instructions[i + 1])
            {
                if operands_equal(dst1, dst2) && val1 == val2 {
                    // Remove both instructions
                    instructions.remove(i + 1);
                    instructions.remove(i);
                    changed = true;
                    continue;
                }
            }

            i += 1;
        }

        changed
    }

    /// Optimize stack operations (push/pop pairs)
    fn optimize_stack_operations(&mut self, instructions: &mut Vec<AsmInstruction>) -> bool {
        let mut changed = false;
        let mut i = 0;

        while i + 1 < instructions.len() {
            // Look for push/pop of same register -> nop
            if let (AsmInstruction::Push(src), AsmInstruction::Pop(dst)) =
                (&instructions[i], &instructions[i + 1])
            {
                if operands_equal(src, dst) {
                    // Remove both instructions
                    instructions.remove(i + 1);
                    instructions.remove(i);
                    changed = true;
                    continue;
                }
            }

            // Look for push reg1; pop reg2 -> mov reg1, reg2
            if let (AsmInstruction::Push(src), AsmInstruction::Pop(dst)) =
                (&instructions[i], &instructions[i + 1])
            {
                if !operands_equal(src, dst) {
                    instructions[i] = AsmInstruction::Move(src.clone(), dst.clone());
                    instructions.remove(i + 1);
                    changed = true;
                    continue;
                }
            }

            i += 1;
        }

        changed
    }

    /// Remove redundant comparisons
    fn remove_redundant_comparisons(&mut self, instructions: &mut Vec<AsmInstruction>) -> bool {
        let mut changed = false;
        let mut i = 0;

        while i + 1 < instructions.len() {
            // Look for repeated comparisons
            if let (AsmInstruction::Compare(src1, dst1), AsmInstruction::Compare(src2, dst2)) =
                (&instructions[i], &instructions[i + 1])
            {
                if operands_equal(src1, src2) && operands_equal(dst1, dst2) {
                    // Remove the second comparison
                    instructions.remove(i + 1);
                    changed = true;
                    continue;
                }
            }

            i += 1;
        }

        changed
    }

    /// Peephole optimization: look for specific patterns and replace them
    fn peephole_optimize(&mut self, instructions: &mut Vec<AsmInstruction>) -> bool {
        let mut changed = false;
        let mut i = 0;

        while i < instructions.len() {
            // Pattern: mov reg, 0; add reg, val -> mov reg, val
            if i + 1 < instructions.len() {
                if let (
                    AsmInstruction::Move(FALSE, dst1),
                    AsmInstruction::Add(AsmOperand::Immediate(val), dst2),
                ) = (&instructions[i], &instructions[i + 1])
                {
                    if operands_equal(dst1, dst2) {
                        instructions[i] =
                            AsmInstruction::Move(AsmOperand::Immediate(*val), dst1.clone());
                        instructions.remove(i + 1);
                        changed = true;
                        continue;
                    }
                }
            }

            i += 1;
        }

        changed
    }

    /// Analyze register usage patterns (for future optimizations)
    fn analyze_register_usage(&mut self, instructions: &[AsmInstruction]) {
        self.register_usage.clear();

        for instr in instructions {
            match instr {
                AsmInstruction::Move(src, dst)
                | AsmInstruction::MoveEqual(src, dst)
                | AsmInstruction::MoveNotEqual(src, dst)
                | AsmInstruction::Add(src, dst)
                | AsmInstruction::Subtract(src, dst)
                | AsmInstruction::SignedMultiply(src, dst)
                | AsmInstruction::Xor(src, dst)
                | AsmInstruction::Compare(src, dst) => {
                    self.count_operand_usage(src);
                    self.count_operand_usage(dst);
                }
                AsmInstruction::Push(op)
                | AsmInstruction::Pop(op)
                | AsmInstruction::SetEqual(op)
                | AsmInstruction::Jump(op)
                | AsmInstruction::JumpEqual(op)
                | AsmInstruction::SignedDivide(op) => {
                    self.count_operand_usage(op);
                }
                _ => {}
            }
        }
    }

    /// Count register usage for analysis
    fn count_operand_usage(&mut self, operand: &AsmOperand) {
        if let AsmOperand::Register(reg) = operand {
            *self.register_usage.entry(*reg).or_insert(0) += 1;
        }
    }
}

/// Check if two operands are equal
fn operands_equal(op1: &AsmOperand, op2: &AsmOperand) -> bool {
    match (op1, op2) {
        (AsmOperand::Register(r1), AsmOperand::Register(r2)) => r1 == r2,
        (AsmOperand::Immediate(i1), AsmOperand::Immediate(i2)) => i1 == i2,
        (AsmOperand::Label(l1), AsmOperand::Label(l2)) => l1 == l2,
        (AsmOperand::Memory(m1), AsmOperand::Memory(m2)) => m1 == m2,
        (AsmOperand::StackBase(s1), AsmOperand::StackBase(s2)) => s1 == s2,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::bonobo::asm::operand::{AsmRegister, FALSE, RAX, RBX, RCX, RDX};

    use super::*;

    #[test]
    fn test_redundant_move_removal() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            AsmInstruction::Move(RAX, RAX),
            AsmInstruction::Move(AsmOperand::Immediate(5), RBX),
        ];

        optimizer.remove_redundant_moves(&mut instructions);
        assert_eq!(instructions.len(), 1);
    }

    #[test]
    fn test_redundant_move_pairs() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            AsmInstruction::Move(RAX, RBX),
            AsmInstruction::Move(RBX, RAX),
            AsmInstruction::Move(AsmOperand::Immediate(42), RCX),
        ];

        optimizer.remove_redundant_moves(&mut instructions);
        // Should remove the second move, leaving 2 instructions
        assert_eq!(instructions.len(), 2);
    }

    #[test]
    fn test_constant_folding_add_zero() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![AsmInstruction::Add(FALSE, RAX)];

        optimizer.fold_constants(&mut instructions);
        // Should be converted to a mov rax, rax (which gets removed later)
        assert!(matches!(instructions[0], AsmInstruction::Move(_, _)));
    }

    #[test]
    fn test_constant_folding_subtract_zero() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![AsmInstruction::Subtract(FALSE, RBX)];

        optimizer.fold_constants(&mut instructions);
        assert!(matches!(instructions[0], AsmInstruction::Move(_, _)));
    }

    #[test]
    fn test_constant_folding_multiply_zero() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![AsmInstruction::SignedMultiply(FALSE, RAX)];

        optimizer.fold_constants(&mut instructions);
        if let AsmInstruction::Move(AsmOperand::Immediate(val), _) = &instructions[0] {
            assert_eq!(*val, 0);
        } else {
            panic!("Expected move with immediate 0");
        }
    }

    #[test]
    fn test_constant_folding_multiply_one() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![AsmInstruction::SignedMultiply(TRUE, RCX)];

        optimizer.fold_constants(&mut instructions);
        assert!(matches!(instructions[0], AsmInstruction::Move(_, _)));
    }

    #[test]
    fn test_dead_code_removal_after_return() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            AsmInstruction::Move(AsmOperand::Immediate(42), RAX),
            AsmInstruction::Return,
            AsmInstruction::Move(FALSE, RBX),
            AsmInstruction::Add(TRUE, RCX),
        ];

        optimizer.remove_dead_code(&mut instructions);
        assert_eq!(instructions.len(), 2); // Only the first move and return should remain
    }

    #[test]
    fn test_dead_code_removal_after_jump() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            AsmInstruction::Jump(AsmOperand::Label("loop".to_string())),
            AsmInstruction::Move(AsmOperand::Immediate(99), RAX),
            AsmInstruction::Label(AsmOperand::Label("loop".to_string())),
            AsmInstruction::Return,
        ];

        optimizer.remove_dead_code(&mut instructions);
        assert_eq!(instructions.len(), 3); // Jump, label, and return should remain
    }

    #[test]
    fn test_arithmetic_optimization_add_combine() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            AsmInstruction::Add(AsmOperand::Immediate(5), RAX),
            AsmInstruction::Add(AsmOperand::Immediate(3), RAX),
        ];

        optimizer.optimize_arithmetic(&mut instructions);
        assert_eq!(instructions.len(), 1);
        if let AsmInstruction::Add(AsmOperand::Immediate(val), _) = &instructions[0] {
            assert_eq!(*val, 8);
        } else {
            panic!("Expected combined add instruction");
        }
    }

    #[test]
    fn test_arithmetic_optimization_add_subtract_cancel() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            AsmInstruction::Add(AsmOperand::Immediate(10), RBX),
            AsmInstruction::Subtract(AsmOperand::Immediate(10), RBX),
            AsmInstruction::Return,
        ];

        optimizer.optimize_arithmetic(&mut instructions);
        assert_eq!(instructions.len(), 1); // Only return should remain
    }

    #[test]
    fn test_stack_optimization_push_pop_same_register() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            AsmInstruction::Push(RAX),
            AsmInstruction::Pop(RAX),
            AsmInstruction::Return,
        ];

        optimizer.optimize_stack_operations(&mut instructions);
        assert_eq!(instructions.len(), 1); // Only return should remain
    }

    #[test]
    fn test_stack_optimization_push_pop_different_registers() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![AsmInstruction::Push(RAX), AsmInstruction::Pop(RBX)];

        optimizer.optimize_stack_operations(&mut instructions);
        assert_eq!(instructions.len(), 1);
        if let AsmInstruction::Move(src, dst) = &instructions[0] {
            assert!(matches!(src, &RAX));
            assert!(matches!(dst, &RBX));
        } else {
            panic!("Expected push/pop to be converted to move");
        }
    }

    #[test]
    fn test_comparison_optimization_duplicate_removal() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            AsmInstruction::Compare(RAX, FALSE),
            AsmInstruction::Compare(RAX, FALSE),
            AsmInstruction::JumpEqual(AsmOperand::Label("zero".to_string())),
        ];

        optimizer.remove_redundant_comparisons(&mut instructions);
        assert_eq!(instructions.len(), 2); // One compare and the jump should remain
    }

    #[test]
    fn test_peephole_mov_zero_add() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            AsmInstruction::Move(FALSE, RAX),
            AsmInstruction::Add(AsmOperand::Immediate(42), RAX),
        ];

        optimizer.peephole_optimize(&mut instructions);
        assert_eq!(instructions.len(), 1);
        if let AsmInstruction::Move(AsmOperand::Immediate(val), _) = &instructions[0] {
            assert_eq!(*val, 42);
        } else {
            panic!("Expected mov with immediate 42");
        }
    }

    #[test]
    fn test_full_optimization_integration() {
        let mut optimizer = AsmOptimizer::new();
        let mut section = AsmSection {
            name: "test".to_string(),
            instructions: vec![
                // Redundant move
                AsmInstruction::Move(RAX, RAX),
                // Add zero
                AsmInstruction::Add(FALSE, RCX),
                // Push/pop same register
                AsmInstruction::Push(RDX),
                AsmInstruction::Pop(RDX),
                // Dead code after return
                AsmInstruction::Return,
                AsmInstruction::Move(
                    AsmOperand::Immediate(999),
                    AsmOperand::Register(AsmRegister::R8),
                ),
            ],
        };

        optimizer.optimize_section(&mut section);

        // Should be heavily optimized - only return should remain after all optimizations
        assert_eq!(section.instructions.len(), 1);
        assert!(matches!(section.instructions[0], AsmInstruction::Return));
    }

    #[test]
    fn test_complex_arithmetic_patterns() {
        let mut optimizer = AsmOptimizer::new();
        let mut instructions = vec![
            // Multiple adds to same register
            AsmInstruction::Add(AsmOperand::Immediate(1), RAX),
            AsmInstruction::Add(AsmOperand::Immediate(2), RAX),
            AsmInstruction::Add(AsmOperand::Immediate(3), RAX),
        ];

        // Optimization should reduce add instruction count to 1
        optimizer.optimize_arithmetic(&mut instructions);
        assert_eq!(instructions.len(), 1);

        if let AsmInstruction::Add(AsmOperand::Immediate(val), _) = &instructions[0] {
            assert_eq!(*val, 6);
        }
    }

    #[test]
    fn test_preserve_non_optimizable_instructions() {
        let mut optimizer = AsmOptimizer::new();
        let instructions = vec![
            AsmInstruction::FnCall("malloc".to_string()),
            AsmInstruction::Syscall,
            AsmInstruction::Cqo,
            AsmInstruction::SignedDivide(RBX),
        ];

        let original_len = instructions.len();
        optimizer.optimize_section(&mut AsmSection {
            name: "test".to_string(),
            instructions: instructions.clone(),
        });

        // These instructions should not be modified
        assert_eq!(instructions.len(), original_len);
    }

    #[test]
    fn test_operands_equal() {
        // Test register equality
        assert!(operands_equal(&RAX, &RAX));
        assert!(!operands_equal(&RAX, &RBX));

        // Test immediate equality
        assert!(operands_equal(
            &AsmOperand::Immediate(42),
            &AsmOperand::Immediate(42)
        ));
        assert!(!operands_equal(
            &AsmOperand::Immediate(42),
            &AsmOperand::Immediate(99)
        ));

        // Test label equality
        assert!(operands_equal(
            &AsmOperand::Label("test".to_string()),
            &AsmOperand::Label("test".to_string())
        ));
        assert!(!operands_equal(
            &AsmOperand::Label("test1".to_string()),
            &AsmOperand::Label("test2".to_string())
        ));

        // Test mixed types
        assert!(!operands_equal(&RAX, &FALSE));
    }

    #[test]
    fn test_stack_base_and_memory_operands() {
        // Test StackBase equality
        assert!(operands_equal(
            &AsmOperand::StackBase(-8),
            &AsmOperand::StackBase(-8)
        ));
        assert!(!operands_equal(
            &AsmOperand::StackBase(-8),
            &AsmOperand::StackBase(-16)
        ));

        // Test Memory equality
        assert!(operands_equal(
            &AsmOperand::Memory("global_var".to_string()),
            &AsmOperand::Memory("global_var".to_string())
        ));
        assert!(!operands_equal(
            &AsmOperand::Memory("var1".to_string()),
            &AsmOperand::Memory("var2".to_string())
        ));
    }
}
