# Register Specification Format (RSF)

The register specification format (RSF) describes **registers** and their
organization within **blocks** that collectively form a hardware/software
interface.

The primary purpose of RSF is to unambiguously describe a register-based
hardware interface. This allows for the development of tooling on both
the hardware side of the interface for things like HDL generation, and
the software side of the interface for things language bindings for driver
code.

RSF comes with

- A parser
- A AST form and library interfaces for working with RSF ASTs.
- A verified model form and library interfaces for writing tooling around RSF.
- A Rust code generator for creating a register programming interface (RPI).

# Registers

Consider a simple register.

```rsf
/// Configuration for an Ethernet physical interface (phy).
register<32> PhyConfig {
    /// Data rate the phy is operating at.
    speed: rw ethernet::DataRate @ 0,

    /// Signal reach the phy is configured for.
    reach: rw ethernet::Reach @ 8,

    /// Number of lanes the phy is using.
    lanes: rw Lanes @ 16,

    /// Type of forward error correction to use.
    fec: rw ethernet::Fec @ 20,

    /// Type of modulation used on the wire.
    modulation: rw cei::Modulation @ 24,
}
```

This register is 32 bits wide as indicated by `<32>`. The name of the
register is `PhyConfig` and it has 5 fields. Each field has a

* name
* mode (read-only, write-only, read-write, reserved)
* type
* offset

The `ethernet::DataRate` notation means that the `DataRate` type is in the
`ethernet` module. Modules can be used with use statements such as

```rsf
use ethernet;
use cei;
```

For fields, the `@ 8` notation means that the field is located at an offset of
8 bits.

## Enums

The following is an example of an enumeration.

```rsf
/// Data rate specification.
enum<2> DataRate {
	/// 50 Gigabits per second.
	G50 = 0b00,

	/// 100 Gigabits per second.
	G100 = 0b01,

	/// 200 Gigabits per second.
	G200 = 0b10,

	/// 400 Gigabites per second.
	G400 = 0b11,
}
```
This enum is 2 bits wide indicated by the `<2>` notation. An enumeration is a
list of alternates, each assigned a numeric value. Values are assigned as
literals and can be in base 2 (`0bXXX`), base 16 (`0xXXX`) or base 10 (`XXX`).

## Blocks

Blocks are a collection of registers and sub-blocks.

```rsf
block Phy {
    /// Configuration register.
    config: PhyConfig @ 0x200,

    /// Status register.
    status: PhyStatus @ 0x400,
}
```

Elements of a block are called **components**. Components are syntactically
similar to register fields. One imporant difference is that component offets
are in bytes and not bits. A component can be either a register or a nested
block. Blocks can be nested arbitrarily deep.

Blocks also support **arrays** of components.

```rsf
block Nic {
    /// A block for each of the four phys.
    phys: Phy[4; 0x1000] @ 0x6000,
}
```

Here the `Phy` component is an array of 4 `Phy`s, where each `Phy` is spaced
`0x1000` bytes apart starting at an offset of `0x6000` bytes.
