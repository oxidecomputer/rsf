//! This module contains a register database type. The register database
//! stores resolved register definitions (see model.rs on what resolved means)
//! that are indexed by both address and name. This provides an interface for
//! looking up and using register types for interactive programs.

use crate::model::Register;
use iddqd::{BiHashItem, BiHashMap, bi_upcast};
use std::{
    hash::Hash,
    ops::{Deref, DerefMut},
};

/// A register db entry. T should be u8, u16, u32, or u64. But there is not a
/// way to express this constraint that I am aware of.
pub struct Entry<T: Copy + Eq + Hash> {
    pub name: String,
    pub address: T,
    pub register: Register,
}

impl<T: Copy + Eq + Hash> BiHashItem for Entry<T> {
    type K1<'a>
        = T
    where
        T: 'a;
    type K2<'a>
        = &'a str
    where
        T: 'a;

    fn key1(&self) -> Self::K1<'_> {
        self.address
    }

    fn key2(&self) -> Self::K2<'_> {
        self.name.as_str()
    }

    bi_upcast!();
}

/// A register DB indexed on address and name.
pub struct RegisterDb<T: Copy + Eq + Hash>(BiHashMap<Entry<T>>);

impl<T: Copy + Eq + Hash> Deref for RegisterDb<T> {
    type Target = BiHashMap<Entry<T>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Copy + Eq + Hash> DerefMut for RegisterDb<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Copy + Eq + Hash> Default for RegisterDb<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Copy + Eq + Hash> RegisterDb<T> {
    pub fn new() -> Self {
        Self(BiHashMap::new())
    }
}
