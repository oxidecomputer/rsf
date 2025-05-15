#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

use expectorate::assert_contents;
use rsf::rust_codegen::{AddrType, ValueType};

// Test code generation in terms of expected syntax.
#[test]
fn test_codegen() {
    let output = match rsf::rust_codegen::codegen(
        "examples/nic.rsf".into(),
        AddrType::U32,
        ValueType::U32,
    ) {
        Ok(out) => out,
        Err(ref e) => {
            panic!("codegen failed: {e}");
        }
    };
    assert_contents("test_data/nic_rpi.rs", &output);
}

// Kersplat generated code right here!
#[allow(dead_code)]
#[cfg(feature = "test_generated")]
mod generated {
    include!("../test_data/nic_rpi.rs");
}

// Run a test program against generated code
#[cfg(feature = "test_generated")]
#[test]
fn run_generated_code() -> anyhow::Result<()> {
    use generated::*;
    use rsf::rust_rpi::{DummyPlatform, RegisterInstance};

    // Initialize the underlying platform. For testing this is just a
    // dummy platform.
    let platform = DummyPlatform::default();

    // Create the RPI client
    let rpi = Client::default();

    // Poke at some config
    rpi.phys(1)?
        .config()
        .update(&platform, |c: &mut PhyConfig| {
            c.set_speed(ethernet::DataRate::G50);
        })
        .unwrap();

    // Read some config, status
    let config = rpi.phys(1)?.config().read(&platform).unwrap();
    assert_eq!(config.get_speed(), ethernet::DataRate::G50);

    let status = rpi.phys(2)?.status().read(&platform).unwrap();
    assert!(!status.get_carrier());

    Ok(())
}
