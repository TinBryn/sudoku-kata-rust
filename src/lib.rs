use std::collections::{BTreeMap, VecDeque};

pub use board::*;
pub use lookups::*;

pub mod board;
pub mod lookups;

#[derive(Clone)]
pub struct Grouping<K, T> {
    key: K,
    pub data: Vec<T>,
}

impl<K, T> Grouping<K, T> {
    pub fn count<F>(&self, pred: F) -> usize
        where
            F: FnMut(&&T) -> bool,
    {
        self.data.iter().filter(pred).count()
    }

    pub fn any<P: FnMut(&T) -> bool>(&self, pred: P) -> bool {
        self.data.iter().any(pred)
    }

    pub fn first(&self) -> Option<&T> {
        self.data.first()
    }
}


#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct List<T> {
    pub data: Vec<T>,
}

impl<T> List<T> {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn add(&mut self, item: T) {
        self.data.push(item);
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn at(&self, loc: usize) -> &T {
        &self.data[loc]
    }
}

impl<T> Default for List<T> {
    fn default() -> Self {
        List::new()
    }
}

#[derive(Clone)]
pub struct CellGroup {
    pub discriminator: usize,
    pub description: String,
    pub index: usize,
    pub row: usize,
    pub col: usize
}

pub struct TwoDigitMask {
    pub mask: usize,
    pub description: String,
    pub cells: Grouping<usize, CellGroup>
}

pub struct NMaskGroups {
    pub mask: usize,
    pub description: String,
    pub cells: Grouping<usize, CellGroup>,
    pub cells_with_masks: Vec<CellGroup>
}

impl NMaskGroups {
    pub fn mask(&self) -> &usize { &self.mask }
}

pub fn group_iter_by_key<I, T, F, K>(vec: I, mapping: F) -> Vec<Grouping<K, T>>
    where
        F: Fn(T) -> K,
        K: PartialEq + PartialOrd + Ord + Copy,
        I: Iterator<Item = T>,
        T: Clone
{
    let mut map: BTreeMap<K, Vec<T>> = BTreeMap::new();
    for item in vec {
        let key = mapping(item.clone());
        match &mut map.get_mut(&key) {
            Some(list) => {
                list.push(item);
            }
            None => {
                map.insert(key, vec![item]);
            }
        };
    }

    Vec::new()
}

pub struct Queues {
    pub index_queue_1: VecDeque<usize>,
    pub index_queue_2: VecDeque<usize>,
    pub digit_queue_1: VecDeque<usize>,
    pub digit_queue_2: VecDeque<usize>,
}
