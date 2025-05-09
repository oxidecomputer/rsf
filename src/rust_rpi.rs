//! Rust register programming interface (RPI)

use std::marker::PhantomData;

use anyhow::Result;

pub trait Platform<AddrType> {
    fn read<T: Default>(&self, addr: AddrType) -> Result<T>;
    fn write<T: Default>(&self, addr: AddrType, value: T) -> Result<()>;
}

pub trait RegisterInstance<T, AddrType> {
    fn read(&self, platform: &impl Platform<AddrType>) -> Result<T>;
    fn write(&self, platform: &impl Platform<AddrType>, value: T)
    -> Result<()>;
    fn update(
        &self,
        platform: &impl Platform<AddrType>,
        f: fn(&mut T) -> Result<()>,
    ) -> Result<()>;
}

#[derive(Default)]
pub struct DummyPlatform<AddrType> {
    phantom_data: PhantomData<AddrType>,
}
impl<AddrType> Platform<AddrType> for DummyPlatform<AddrType> {
    fn read<T: Default>(&self, _addr: AddrType) -> Result<T> {
        Ok(T::default())
    }
    fn write<T: Default>(&self, _addr: AddrType, _value: T) -> Result<()> {
        Ok(())
    }
}
