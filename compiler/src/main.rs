#![feature(decl_macro)]
#![feature(iter_next_chunk)]
#![feature(assert_matches)]
#![feature(iter_array_chunks)]

use std::error::Error;
use std::fs;
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
    #[arg(long, default_value = "./out/out.wat")]
    wat_file: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let source = fs::read_to_string(args.input_file)?;
    let lib = match parse::parse(source) {
        Err(parse::Error::PestParseFailed(err)) => {
            eprintln!("parse error: \n{err}");
            return Err(err.into());
        }
        Err(err) => return Err(err.into()),
        Ok(lib) => lib,
    };
    let module = compile::compile(lib)?;
    if let Some(parent) = args.out_file.parent() {
        fs::create_dir_all(parent)?;
    }
    let module_bytes = module.finish();
    fs::write(args.out_file, &module_bytes)?;
    let wat_text = wasmprinter::print_bytes(&module_bytes)?;
    fs::write(args.wat_file, wat_text)?;

    Ok(())
}
