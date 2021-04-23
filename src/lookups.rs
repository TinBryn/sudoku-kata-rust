fn mask_to_ones_count() -> HashMap<i32, i32> {
    let mut mask_to_ones_count = HashMap::new();
    mask_to_ones_count.insert(0, 0);
    for i in 1..(1 << 9) {
        let smaller = i >> 1;
        let increment = i & 1;
        mask_to_ones_count.insert(i, mask_to_ones_count[&smaller] + increment);
    }
    mask_to_ones_count
}

fn single_bit_to_index() -> HashMap<i32, i32> {
    let mut single_bit_to_index = HashMap::new();
    for i in 0..9 {
        single_bit_to_index.insert(1 << i, i);
    }
    single_bit_to_index
}

fn all_ones() -> i32 { (1 << 9) - 1 }

use std::collections::HashMap;

pub struct LookupStructures {
    mask_to_ones_count: HashMap<i32, i32>,
    single_bit_to_index: HashMap<i32, i32>,
    all_ones: i32,
}

impl Default for LookupStructures {
    fn default() -> Self {
        LookupStructures::new()
    }
}

impl LookupStructures {
    pub fn new() -> Self {
        LookupStructures {
            mask_to_ones_count: mask_to_ones_count(),
            single_bit_to_index: single_bit_to_index(),
            all_ones: all_ones(),
        }
    }
    pub fn mask_to_ones_count(&self) -> &HashMap<i32, i32> {
        &self.mask_to_ones_count
    }
    pub fn single_bit_to_index(&self) -> &HashMap<i32, i32> {
        &self.single_bit_to_index
    }
    pub fn all_ones(&self) -> i32 {
        self.all_ones
    }
}