use bitset::BitSet;
use rsf::rust_rpi;
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
    pub fn reset(&mut self) {
        self.0 = BitSet::<32>::ZERO;
    }
}
impl From<u32> for PhyConfig {
    fn from(value: u32) -> Self {
        Self(BitSet::<32>::from(value))
    }
}
impl From<PhyConfig> for u32 {
    fn from(value: PhyConfig) -> Self {
        u32::from(value.0)
    }
}
///Instance of a [`PhyConfig`]
pub struct PhyConfigInstance {
    pub addr: u32,
}
impl rust_rpi::RegisterInstance<PhyConfig, u32, u32> for PhyConfigInstance {
    fn cons(&self) -> PhyConfig {
        let mut v = PhyConfig::default();
        v.reset();
        v
    }
    fn read<P: rust_rpi::Platform<u32, u32>>(
        &self,
        platform: &P,
    ) -> Result<PhyConfig, P::Error> {
        platform.read(self.addr)
    }
    fn write<P: rust_rpi::Platform<u32, u32>>(
        &self,
        platform: &P,
        value: PhyConfig,
    ) -> Result<(), P::Error> {
        platform.write(self.addr, value)
    }
    fn try_update<
        P: rust_rpi::Platform<u32, u32>,
        F: FnOnce(&mut PhyConfig) -> Result<(), P::Error>,
    >(&self, platform: &P, f: F) -> Result<(), P::Error> {
        let mut value = self.read(platform)?;
        f(&mut value)?;
        self.write(platform, value)
    }
    fn update<P: rust_rpi::Platform<u32, u32>, F: FnOnce(&mut PhyConfig)>(
        &self,
        platform: &P,
        f: F,
    ) -> Result<(), P::Error> {
        let mut value = self.read(platform)?;
        f(&mut value);
        self.write(platform, value)
    }
}
#[derive(Default, Debug)]
/// Status of an Ethernet physical interface (phy).
pub struct PhyStatus(BitSet<32>);
impl PhyStatus {
    /// Indicates if a carrier signal is detected.
    pub fn get_carrier(&self) -> bool {
        bool::from(self.0.get_field::<1, 0>().unwrap())
    }
    /// Indicates if a signal error has been recieved by the MAU.
    pub fn get_signal_error(&self) -> bool {
        bool::from(self.0.get_field::<1, 1>().unwrap())
    }
    /// Indicates that data in the signal received from the MAU is valid.
    pub fn get_data_valid(&self) -> bool {
        bool::from(self.0.get_field::<1, 2>().unwrap())
    }
    pub fn reset(&mut self) {
        self.0 = BitSet::<32>::ZERO;
    }
}
impl From<u32> for PhyStatus {
    fn from(value: u32) -> Self {
        Self(BitSet::<32>::from(value))
    }
}
impl From<PhyStatus> for u32 {
    fn from(value: PhyStatus) -> Self {
        u32::from(value.0)
    }
}
///Instance of a [`PhyStatus`]
pub struct PhyStatusInstance {
    pub addr: u32,
}
impl rust_rpi::RegisterInstance<PhyStatus, u32, u32> for PhyStatusInstance {
    fn cons(&self) -> PhyStatus {
        let mut v = PhyStatus::default();
        v.reset();
        v
    }
    fn read<P: rust_rpi::Platform<u32, u32>>(
        &self,
        platform: &P,
    ) -> Result<PhyStatus, P::Error> {
        platform.read(self.addr)
    }
    fn write<P: rust_rpi::Platform<u32, u32>>(
        &self,
        platform: &P,
        value: PhyStatus,
    ) -> Result<(), P::Error> {
        platform.write(self.addr, value)
    }
    fn try_update<
        P: rust_rpi::Platform<u32, u32>,
        F: FnOnce(&mut PhyStatus) -> Result<(), P::Error>,
    >(&self, platform: &P, f: F) -> Result<(), P::Error> {
        let mut value = self.read(platform)?;
        f(&mut value)?;
        self.write(platform, value)
    }
    fn update<P: rust_rpi::Platform<u32, u32>, F: FnOnce(&mut PhyStatus)>(
        &self,
        platform: &P,
        f: F,
    ) -> Result<(), P::Error> {
        let mut value = self.read(platform)?;
        f(&mut value);
        self.write(platform, value)
    }
}
#[derive(Debug, Default)]
/// Metadata value
pub struct Metadata([u8; 32]);
///Instance of a [`Metadata`]
pub struct MetadataInstance {
    pub msel_id: u32,
}
#[derive(Debug, Default)]
/// Firmware instruction
pub struct FirmwareInstruction([u8; 32]);
///Instance of a [`FirmwareInstruction`]
pub struct FirmwareInstructionInstance {
    pub msel_id: u32,
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
    type Error = rust_rpi::OutOfRange;
    fn try_from(value: BitSet<2>) -> Result<Self, Self::Error> {
        Self::try_from(u8::from(value))
            .map_err(|_| rust_rpi::OutOfRange::EnumValueOutOfRange)
    }
}
/// Phy registers.
#[derive(Default, Debug)]
pub struct PhyInstance {
    pub addr: u32,
}
/// Firmware block
#[derive(Default, Debug)]
pub struct FirmwareInstance {
    pub addr: u32,
}
/// Register programming interface for this NIC.
#[derive(Default, Debug)]
pub struct Client {
    pub addr: u32,
}
impl Client {
    /// The NIC's version info
    pub fn version(&self) -> version::VersionInfoInstance {
        version::VersionInfoInstance {
            addr: self.addr + 0x100,
        }
    }
    /// A block for each of the four phys.
    pub fn phys(&self, index: u32) -> Result<PhyInstance, rust_rpi::OutOfRange> {
        if index > 4 {
            return Err(rust_rpi::OutOfRange::IndexOutOfRange);
        }
        Ok(PhyInstance {
            addr: self.addr + 0x6000 + (index * 0x1000),
        })
    }
    /// The NIC's firmware.
    pub fn firmware(&self) -> FirmwareInstance {
        FirmwareInstance {
            addr: self.addr + 0x10000,
        }
    }
}
impl FirmwareInstance {
    /// Metadata section of firmware
    pub fn metadata(&self) -> MetadataInstance {
        MetadataInstance { msel_id: 0 }
    }
    /// Instruction section of firmware
    pub fn instructions(&self) -> FirmwareInstructionInstance {
        FirmwareInstructionInstance {
            msel_id: 1,
        }
    }
}
impl PhyInstance {
    /// The Phy's version info
    pub fn version(&self) -> version::VersionInfoInstance {
        version::VersionInfoInstance {
            addr: self.addr,
        }
    }
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
    use rsf::rust_rpi;
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
        type Error = rust_rpi::OutOfRange;
        fn try_from(value: BitSet<1>) -> Result<Self, Self::Error> {
            Self::try_from(u8::from(value))
                .map_err(|_| rust_rpi::OutOfRange::EnumValueOutOfRange)
        }
    }
}
pub mod ethernet {
    use super::version;
    use bitset::BitSet;
    use rsf::rust_rpi;
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
        type Error = rust_rpi::OutOfRange;
        fn try_from(value: BitSet<3>) -> Result<Self, Self::Error> {
            Self::try_from(u8::from(value))
                .map_err(|_| rust_rpi::OutOfRange::EnumValueOutOfRange)
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
        type Error = rust_rpi::OutOfRange;
        fn try_from(value: BitSet<2>) -> Result<Self, Self::Error> {
            Self::try_from(u8::from(value))
                .map_err(|_| rust_rpi::OutOfRange::EnumValueOutOfRange)
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
        type Error = rust_rpi::OutOfRange;
        fn try_from(value: BitSet<2>) -> Result<Self, Self::Error> {
            Self::try_from(u8::from(value))
                .map_err(|_| rust_rpi::OutOfRange::EnumValueOutOfRange)
        }
    }
    /** Test block

 This block isn't referenced anywhere.  It exists simply to exercise the
 importation of a shared subblock (i.e., "version") at multiple levels.*/
    #[derive(Default, Debug)]
    pub struct TestInstance {
        pub addr: u32,
    }
    impl TestInstance {
        /// version info
        pub fn version(&self) -> version::VersionInfoInstance {
            version::VersionInfoInstance {
                addr: self.addr,
            }
        }
    }
}
pub mod version {
    use bitset::BitSet;
    use rsf::rust_rpi;
    #[derive(Default, Debug)]
    /// Version number
    pub struct Version(BitSet<32>);
    impl Version {
        /// Version number
        pub fn get_value(&self) -> BitSet<32> {
            self.0.get_field::<32, 0>().unwrap()
        }
        pub fn reset(&mut self) {
            self.0 = BitSet::<32>::ZERO;
        }
    }
    impl From<u32> for Version {
        fn from(value: u32) -> Self {
            Self(BitSet::<32>::from(value))
        }
    }
    impl From<Version> for u32 {
        fn from(value: Version) -> Self {
            u32::from(value.0)
        }
    }
    ///Instance of a [`Version`]
    pub struct VersionInstance {
        pub addr: u32,
    }
    impl rust_rpi::RegisterInstance<Version, u32, u32> for VersionInstance {
        fn cons(&self) -> Version {
            let mut v = Version::default();
            v.reset();
            v
        }
        fn read<P: rust_rpi::Platform<u32, u32>>(
            &self,
            platform: &P,
        ) -> Result<Version, P::Error> {
            platform.read(self.addr)
        }
        fn write<P: rust_rpi::Platform<u32, u32>>(
            &self,
            platform: &P,
            value: Version,
        ) -> Result<(), P::Error> {
            platform.write(self.addr, value)
        }
        fn try_update<
            P: rust_rpi::Platform<u32, u32>,
            F: FnOnce(&mut Version) -> Result<(), P::Error>,
        >(&self, platform: &P, f: F) -> Result<(), P::Error> {
            let mut value = self.read(platform)?;
            f(&mut value)?;
            self.write(platform, value)
        }
        fn update<P: rust_rpi::Platform<u32, u32>, F: FnOnce(&mut Version)>(
            &self,
            platform: &P,
            f: F,
        ) -> Result<(), P::Error> {
            let mut value = self.read(platform)?;
            f(&mut value);
            self.write(platform, value)
        }
    }
    /// Component version info
    #[derive(Default, Debug)]
    pub struct VersionInfoInstance {
        pub addr: u32,
    }
    impl VersionInfoInstance {
        /// Version register.
        pub fn version(&self) -> VersionInstance {
            VersionInstance { addr: self.addr }
        }
    }
}
