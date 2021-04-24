fn mask_to_ones_count() -> HashMap<usize, usize> {
    let mut mask_to_ones_count = HashMap::new();
    mask_to_ones_count.insert(0, 0);
    for i in 1..(1 << 9) {
        let smaller = i >> 1;
        let increment = i & 1;
        mask_to_ones_count.insert(i, mask_to_ones_count[&smaller] + increment);
    }
    mask_to_ones_count
}

fn single_bit_to_index() -> HashMap<usize, usize> {
    let mut single_bit_to_index = HashMap::new();
    for i in 0..9 {
        single_bit_to_index.insert(1 << i, i);
    }
    single_bit_to_index
}

fn all_ones() -> usize { (1 << 9) - 1 }

use std::collections::HashMap;

pub struct LookupStructures {
    mask_to_ones_count: HashMap<usize, usize>,
    single_bit_to_index: HashMap<usize, usize>,
    all_ones: usize,
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
    pub fn mask_to_ones_count(&self) -> &HashMap<usize, usize> {
        &self.mask_to_ones_count
    }
    pub fn single_bit_to_index(&self) -> &HashMap<usize, usize> {
        &self.single_bit_to_index
    }
    pub fn all_ones(&self) -> usize {
        self.all_ones
    }
}