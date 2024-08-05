#![feature(decl_macro)]
#![feature(iter_next_chunk)]
#![feature(assert_matches)]
#![feature(iter_array_chunks)]

use std::error::Error;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use clap::Parser;

mod ast;
mod compile;
mod parse;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    input_file: PathBuf,
    #[arg(short, long, default_value = "./out/out.wasm")]
    out_file: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let source = fs::read_to_string(args.input_file)?;
    let lib = parse::parse(source)?;
    let module = compile::compile(lib)?;
    if let Some(parent) = args.out_file.parent() {
        fs::create_dir_all(parent)?;
    }
    let mut out_file = File::options()
        .write(true)
        .create(true)
        .truncate(true)
        .open(args.out_file)?;
    out_file.write_all(&module.finish())?;
    Ok(())
}
