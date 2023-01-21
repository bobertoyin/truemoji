#![doc = include_str!("../README.md")]

use clap::Parser;

mod cli;
mod table;
use cli::Args;

fn main() {
    let args = Args::parse();
    args.execute_cmd();
}
