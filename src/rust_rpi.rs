//! Rust register programming interface (RPI)

use anyhow::Result;

pub trait Platform<AddrType> {
    fn read<T>(&self, addr: AddrType) -> Result<T>;
    fn write<T>(&self, addr: AddrType, value: T) -> Result<()>;
}

pub trait RegisterInstance<T> {
    fn read(&self) -> Result<T>;
    fn write(&self, value: T) -> Result<()>;
    fn update(&self, f: fn(&mut T) -> Result<()>);
}
