use std::collections::BTreeMap;

pub mod board;
pub mod lookups;

pub use lookups::*;
pub use board::*;

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
    pub discriminator: i32,
    pub description: String,
    pub index: i32,
    pub row: i32,
    pub col: i32
}

pub struct TwoDigitMask {
    pub mask: i32,
    pub description: String,
    pub cells: Grouping<i32, CellGroup>
}

pub struct NMaskGroups {
    pub mask: i32,
    pub description: String,
    pub cells: Grouping<i32, CellGroup>,
    pub cells_with_masks: Vec<CellGroup>
}

impl NMaskGroups {
    pub fn mask(&self) -> &i32 { &self.mask }
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
