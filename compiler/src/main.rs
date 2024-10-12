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
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let module_name = args.input_file.to_string_lossy().replace(".myth", "");
    let source = fs::read_to_string(args.input_file)?;
    let lib = match parse::parse(&source) {
        Err(parse::Error::PestParseFailed(err)) => {
            eprintln!("parse error: \n{err}");
            return Err(err.into());
        }
        Err(err) => return Err(err.into()),
        Ok(lib) => lib,
    };
    let module = compile::compile(lib)?;
    let out_file: PathBuf = format!("./out/{module_name}.wasm").into();
    let wat_file: PathBuf = format!("./out/{module_name}.wat").into();
    if let Some(parent) = out_file.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(out_file, &module)?;
    let wat_text = wasmprinter::print_bytes(&module)?;
    fs::write(wat_file, wat_text)?;

    Ok(())
}
