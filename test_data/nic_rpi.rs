use bitset::BitSet;
use anyhow::Result;
#[derive(Default)]
pub struct PhyConfig(BitSet<32>);
impl PhyConfig {
    pub fn get_speed(&self) -> ethernet::DataRate {
        self.0.get_field::<2, 0>().unwrap().try_into().unwrap()
    }
    pub fn set_speed(&mut self, data__: ethernet::DataRate) {
        self.0.set_field::<2, 0>(data__.into()).unwrap();
    }
    pub fn get_reach(&self) -> ethernet::Reach {
        self.0.get_field::<3, 8>().unwrap().try_into().unwrap()
    }
    pub fn set_reach(&mut self, data__: ethernet::Reach) {
        self.0.set_field::<3, 8>(data__.into()).unwrap();
    }
    pub fn get_lanes(&self) -> Lanes {
        self.0.get_field::<2, 16>().unwrap().try_into().unwrap()
    }
    pub fn set_lanes(&mut self, data__: Lanes) {
        self.0.set_field::<2, 16>(data__.into()).unwrap();
    }
    pub fn get_fec(&self) -> ethernet::Fec {
        self.0.get_field::<2, 20>().unwrap().try_into().unwrap()
    }
    pub fn set_fec(&mut self, data__: ethernet::Fec) {
        self.0.set_field::<2, 20>(data__.into()).unwrap();
    }
    pub fn get_modulation(&self) -> cei::Modulation {
        self.0.get_field::<1, 24>().unwrap().try_into().unwrap()
    }
    pub fn set_modulation(&mut self, data__: cei::Modulation) {
        self.0.set_field::<1, 24>(data__.into()).unwrap();
    }
}
pub struct PhyConfigInstance {
    addr: u32,
}
impl rust_rpi::RegisterInstance<PhyConfig, u32> for PhyConfigInstance {
    fn read(&self, platform: &impl rust_rpi::Platform<u32>) -> Result<PhyConfig> {
        platform.read(self.addr)
    }
    fn write(
        &self,
        platform: &impl rust_rpi::Platform<u32>,
        value: PhyConfig,
    ) -> Result<()> {
        platform.write(self.addr, value)
    }
    fn update(
        &self,
        platform: &impl rust_rpi::Platform<u32>,
        f: fn(&mut PhyConfig) -> Result<()>,
    ) -> Result<()> {
        let mut value = self.read(platform)?;
        f(&mut value)?;
        self.write(platform, value)
    }
}
#[derive(Default)]
pub struct PhyStatus(BitSet<32>);
impl PhyStatus {
    pub fn get_carrier(&self) -> bool {
        self.0.get_field::<1, 0>().unwrap().to_bool()
    }
    pub fn get_signal_error(&self) -> bool {
        self.0.get_field::<1, 1>().unwrap().to_bool()
    }
    pub fn get_data_valid(&self) -> bool {
        self.0.get_field::<1, 2>().unwrap().to_bool()
    }
}
pub struct PhyStatusInstance {
    addr: u32,
}
impl rust_rpi::RegisterInstance<PhyStatus, u32> for PhyStatusInstance {
    fn read(&self, platform: &impl rust_rpi::Platform<u32>) -> Result<PhyStatus> {
        platform.read(self.addr)
    }
    fn write(
        &self,
        platform: &impl rust_rpi::Platform<u32>,
        value: PhyStatus,
    ) -> Result<()> {
        platform.write(self.addr, value)
    }
    fn update(
        &self,
        platform: &impl rust_rpi::Platform<u32>,
        f: fn(&mut PhyStatus) -> Result<()>,
    ) -> Result<()> {
        let mut value = self.read(platform)?;
        f(&mut value)?;
        self.write(platform, value)
    }
}
#[derive(num_enum::TryFromPrimitive)]
#[repr(u8)]
pub enum Lanes {
    Single = 0b000,
    L2 = 0b001,
    L4 = 0b010,
    F2 = 0b011,
    F4 = 0b100,
}
impl From<Lanes> for BitSet<2> {
    fn from(value: Lanes) -> BitSet<2> {
        BitSet::<2>::try_from(value as u8).unwrap()
    }
}
impl TryFrom<BitSet<2>> for Lanes {
    type Error = anyhow::Error;
    fn try_from(value: BitSet<2>) -> Result<Self> {
        Ok(Self::try_from(value.to_int())?)
    }
}
#[derive(Default)]
pub struct PhyInstance {
    addr: u32,
}
#[derive(Default)]
pub struct Client {
    addr: u32,
}
impl Client {
    pub fn phys(&self, index: u32) -> PhyInstance {
        PhyInstance {
            addr: self.addr + 0x6000 + (index * 0x1000),
        }
    }
}
impl PhyInstance {
    pub fn config(&self) -> PhyConfigInstance {
        PhyConfigInstance {
            addr: self.addr + 0x200,
        }
    }
    pub fn status(&self) -> PhyStatusInstance {
        PhyStatusInstance {
            addr: self.addr + 0x400,
        }
    }
}
pub mod cei {
    use bitset::BitSet;
    use anyhow::Result;
    #[derive(num_enum::TryFromPrimitive)]
    #[repr(u8)]
    pub enum Modulation {
        Nrz = 0b0,
        Pam4 = 0b1,
    }
    impl From<Modulation> for BitSet<1> {
        fn from(value: Modulation) -> BitSet<1> {
            BitSet::<1>::try_from(value as u8).unwrap()
        }
    }
    impl TryFrom<BitSet<1>> for Modulation {
        type Error = anyhow::Error;
        fn try_from(value: BitSet<1>) -> Result<Self> {
            Ok(Self::try_from(value.to_int())?)
        }
    }
}
pub mod ethernet {
    use bitset::BitSet;
    use anyhow::Result;
    #[derive(num_enum::TryFromPrimitive)]
    #[repr(u8)]
    pub enum Reach {
        Kr = 0b000,
        Cr = 0b001,
        Sr = 0b010,
        Dr = 0b011,
        Fr = 0b100,
        Lr = 0b101,
        Er = 0b110,
    }
    impl From<Reach> for BitSet<3> {
        fn from(value: Reach) -> BitSet<3> {
            BitSet::<3>::try_from(value as u8).unwrap()
        }
    }
    impl TryFrom<BitSet<3>> for Reach {
        type Error = anyhow::Error;
        fn try_from(value: BitSet<3>) -> Result<Self> {
            Ok(Self::try_from(value.to_int())?)
        }
    }
    #[derive(num_enum::TryFromPrimitive)]
    #[repr(u8)]
    pub enum Fec {
        None = 0b00,
        Rs = 0b01,
        Fc = 0b10,
    }
    impl From<Fec> for BitSet<2> {
        fn from(value: Fec) -> BitSet<2> {
            BitSet::<2>::try_from(value as u8).unwrap()
        }
    }
    impl TryFrom<BitSet<2>> for Fec {
        type Error = anyhow::Error;
        fn try_from(value: BitSet<2>) -> Result<Self> {
            Ok(Self::try_from(value.to_int())?)
        }
    }
    #[derive(num_enum::TryFromPrimitive)]
    #[repr(u8)]
    pub enum DataRate {
        G50 = 0b00,
        G100 = 0b01,
        G200 = 0b10,
        G400 = 0b11,
    }
    impl From<DataRate> for BitSet<2> {
        fn from(value: DataRate) -> BitSet<2> {
            BitSet::<2>::try_from(value as u8).unwrap()
        }
    }
    impl TryFrom<BitSet<2>> for DataRate {
        type Error = anyhow::Error;
        fn try_from(value: BitSet<2>) -> Result<Self> {
            Ok(Self::try_from(value.to_int())?)
        }
    }
}
