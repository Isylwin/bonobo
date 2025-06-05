use std::collections::{HashMap, HashSet};

use crate::bonobo::asm::{AsmInstruction, AsmOperand};

#[derive(Debug)]
pub struct AsmProgram {
    pub externs: HashSet<String>,
    pub globals: Vec<String>,
    pub text: AsmSection,
    pub data: AsmSection,
    pub env: AsmEnvironment,
    pub labels: HashMap<String, i64>,
}

#[derive(Debug)]
pub struct AsmSection {
    pub name: String,
    pub instructions: Vec<AsmInstruction>,
}

#[derive(Debug)]
pub struct AsmEnvironment {
    pub local_offsets: HashMap<String, i64>,
    pub next_local_offset: i64,
}

impl AsmProgram {
    pub fn new() -> Self {
        // Assume that the user's program always defines the banana entrypoint
        let globals = vec!["banana".into()];
        let externs = HashSet::new();
        let text = AsmSection {
            name: ".text".into(),
            instructions: vec![],
        };
        let data = AsmSection {
            name: ".data".into(),
            instructions: vec![],
        };
        let env = AsmEnvironment::new();
        let labels = HashMap::new();

        AsmProgram {
            externs,
            globals,
            text,
            data,
            env,
            labels,
        }
    }

    pub fn add_instruction(&mut self, instruction: AsmInstruction) {
        self.text.instructions.push(instruction)
    }

    pub fn add_extern(&mut self, external: &str) {
        self.externs.insert(external.into());
    }

    pub fn gen_label(&mut self, prefix: &str) -> AsmOperand {
        let label = match self.labels.get_mut(prefix) {
            Some(v) => {
                *v += 1;
                format!("{}{}", prefix, v)
            }
            None => {
                self.labels.insert(prefix.into(), 0);
                format!("{}0", prefix)
            }
        };
        AsmOperand::Label(label)
    }

    pub fn reset_env(&mut self) {
        self.env = AsmEnvironment::new();
    }
}

impl AsmEnvironment {
    pub fn new() -> Self {
        let local_offsets = HashMap::new();

        AsmEnvironment {
            local_offsets,
            next_local_offset: 0,
        }
    }

    pub fn declare_local(&mut self, identifier: &str) {
        self.local_offsets
            .insert(identifier.into(), self.next_local_offset - 8);
        self.next_local_offset -= 8;
    }

    pub fn get_local(&self, identifier: &str) -> Option<i64> {
        self.local_offsets.get(identifier).copied()
    }
}
