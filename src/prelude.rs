pub use anyhow::{bail, ensure, Context, Result};
pub use approx::*;
pub use lazy_static::lazy_static;
pub use log::*;
pub use ordered_float::OrderedFloat;
pub use rand::rngs::StdRng;
pub use rand::Rng;
pub use rand::SeedableRng;
pub use serde::{Deserialize, Serialize};
pub use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::io::prelude::*;
pub use std::io::Write;
pub use std::ops::Deref;
pub use std::ops::Index;
pub use std::ops::IndexMut;
pub use std::ops::Range;
pub use std::path::{Path, PathBuf};

pub fn project_path(relative_path: impl AsRef<Path>) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(relative_path);
    path
}

pub fn read_from(relative_path: impl AsRef<Path>) -> Result<String> {
    let path = project_path(relative_path);
    Ok(std::fs::read_to_string(path)?)
}

pub fn write_to(relative_path: impl AsRef<Path>, content: &str) -> Result<()> {
    let path = project_path(relative_path);
    std::fs::create_dir_all(path.parent().unwrap())?;
    Ok(std::fs::write(path, content)?)
}

pub fn read_bytes(mut read: impl Read) -> Result<Vec<u8>> {
    let mut buf = Vec::<u8>::new();
    read.read_to_end(&mut buf)?;
    Ok(buf)
}

pub fn read_string(mut read: impl Read) -> Result<String> {
    let mut buf = String::new();
    read.read_to_string(&mut buf)?;
    Ok(buf)
}

pub fn test_env_logger() {
    let _ = env_logger::builder().is_test(true).try_init();
}
