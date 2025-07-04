//! Error definitions

#[derive(Debug, thiserror::Error)]
pub enum OutOfRange {
    // Conversion from integer type to an enumeration from a value that is
    // too large for the enum.
    #[error("Enum value out of range")]
    EnumValueOutOfRange,

    /// The supplied index for a block component array is larger than the
    /// length of the array.
    #[error("Enum value out of range")]
    IndexOutOfRange,

    /// The supplied byte array will not fit into the register
    #[error("Byte array too large")]
    ArrayTooLarge,
}
