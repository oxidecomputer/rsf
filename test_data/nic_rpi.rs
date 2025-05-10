use bitset::BitSet;
use anyhow::Result;
#[derive(Default, Debug)]
/** Configuration for an Ethernet physical interface (phy).

 All field modifications take effect immediately.*/
pub struct PhyConfig(BitSet<32>);
impl PhyConfig {
    /// Data rate the phy is operating at.
    pub fn get_speed(&self) -> ethernet::DataRate {
        self.0.get_field::<2, 0>().unwrap().try_into().unwrap()
    }
    /// Data rate the phy is operating at.
    pub fn set_speed(&mut self, data__: ethernet::DataRate) {
        self.0.set_field::<2, 0>(data__.into()).unwrap();
    }
    /// Signal reach the phy is configured for.
    pub fn get_reach(&self) -> ethernet::Reach {
        self.0.get_field::<3, 8>().unwrap().try_into().unwrap()
    }
    /// Signal reach the phy is configured for.
    pub fn set_reach(&mut self, data__: ethernet::Reach) {
        self.0.set_field::<3, 8>(data__.into()).unwrap();
    }
    /// Number of lanes the phy is using.
    pub fn get_lanes(&self) -> Lanes {
        self.0.get_field::<2, 16>().unwrap().try_into().unwrap()
    }
    /// Number of lanes the phy is using.
    pub fn set_lanes(&mut self, data__: Lanes) {
        self.0.set_field::<2, 16>(data__.into()).unwrap();
    }
    /// Type of forward error correction to use.
    pub fn get_fec(&self) -> ethernet::Fec {
        self.0.get_field::<2, 20>().unwrap().try_into().unwrap()
    }
    /// Type of forward error correction to use.
    pub fn set_fec(&mut self, data__: ethernet::Fec) {
        self.0.set_field::<2, 20>(data__.into()).unwrap();
    }
    /// Type of modulation used on the wire.
    pub fn get_modulation(&self) -> cei::Modulation {
        self.0.get_field::<1, 24>().unwrap().try_into().unwrap()
    }
    /// Type of modulation used on the wire.
    pub fn set_modulation(&mut self, data__: cei::Modulation) {
        self.0.set_field::<1, 24>(data__.into()).unwrap();
    }
}
///Instance of a [`PhyConfig`]
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
#[derive(Default, Debug)]
/// Status of an Ethernet physical interface (phy).
pub struct PhyStatus(BitSet<32>);
impl PhyStatus {
    /// Indicates if a carrier signal is detected.
    pub fn get_carrier(&self) -> bool {
        self.0.get_field::<1, 0>().unwrap().to_bool()
    }
    /// Indicates if a signal error has been recieved by the MAU.
    pub fn get_signal_error(&self) -> bool {
        self.0.get_field::<1, 1>().unwrap().to_bool()
    }
    /// Indicates that data in the signal received from the MAU is valid.
    pub fn get_data_valid(&self) -> bool {
        self.0.get_field::<1, 2>().unwrap().to_bool()
    }
}
///Instance of a [`PhyStatus`]
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
/// Number of lanes per phy.
#[derive(num_enum::TryFromPrimitive, PartialEq, Debug)]
#[repr(u8)]
pub enum Lanes {
    /// One lane per phy
    Single = 0b000,
    /// Two lambda lanes per phy
    L2 = 0b001,
    /// Four lambda lanes per phy
    L4 = 0b010,
    /// Two fiber lanes per phy
    F2 = 0b011,
    /// Four fiber lanes per phy
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
/// Phy registers.
#[derive(Default, Debug)]
pub struct PhyInstance {
    addr: u32,
}
/// Register programming interface for this NIC.
#[derive(Default, Debug)]
pub struct Client {
    addr: u32,
}
impl Client {
    /// A block for each of the four phys.
    pub fn phys(&self, index: u32) -> PhyInstance {
        PhyInstance {
            addr: self.addr + 0x6000 + (index * 0x1000),
        }
    }
}
impl PhyInstance {
    /// Configuration register.
    pub fn config(&self) -> PhyConfigInstance {
        PhyConfigInstance {
            addr: self.addr + 0x200,
        }
    }
    /// Status register.
    pub fn status(&self) -> PhyStatusInstance {
        PhyStatusInstance {
            addr: self.addr + 0x400,
        }
    }
}
pub mod cei {
    use bitset::BitSet;
    use anyhow::Result;
    /// Supported signal modulation types.
    #[derive(num_enum::TryFromPrimitive, PartialEq, Debug)]
    #[repr(u8)]
    pub enum Modulation {
        /// Non return to zero
        Nrz = 0b0,
        /// Pulse amplitude modulation 4
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
    /// Reach of a signal.
    #[derive(num_enum::TryFromPrimitive, PartialEq, Debug)]
    #[repr(u8)]
    pub enum Reach {
        /// Backplane reach.
        Kr = 0b000,
        /// Copper reach.
        Cr = 0b001,
        /// Short range fiber reach.
        Sr = 0b010,
        /// Datacenter fiber reach.
        Dr = 0b011,
        /// Fiber reach.
        Fr = 0b100,
        /// Long range fiber reach.
        Lr = 0b101,
        /// Extended fiber reach.
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
    /// Forward error correction mode.
    #[derive(num_enum::TryFromPrimitive, PartialEq, Debug)]
    #[repr(u8)]
    pub enum Fec {
        /// Fec not enabled
        None = 0b00,
        /// Reed-Solomon
        Rs = 0b01,
        /// Firecode
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
    /// Data rate specification.
    #[derive(num_enum::TryFromPrimitive, PartialEq, Debug)]
    #[repr(u8)]
    pub enum DataRate {
        /// 50 Gigabits per second.
        G50 = 0b00,
        /// 100 Gigabits per second.
        G100 = 0b01,
        /// 200 Gigabits per second.
        G200 = 0b10,
        /// 400 Gigabites per second.
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
