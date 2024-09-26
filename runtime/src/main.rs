use clap::Parser;
use std::error::Error;
use std::path::PathBuf;
use wasmtime::{Engine, Linker, Module, Store, Val};

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    input_file: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let engine = Engine::default();
    let main_module = Module::from_file(&engine, args.input_file)?;
    let gc_module = Module::from_file(&engine, "../gc/target/wasm32-wasip1/release/gc.wasm")?;
    let mut linker = Linker::new(&engine);
    let mut store = Store::new(&engine, ());
    linker.module(&mut store, "gc", &gc_module)?;
    let instance = linker.instantiate(&mut store, &main_module)?;
    let func = instance
        .get_func(&mut store, "_start")
        .ok_or("no _start function")?;
    let mut retval = [Val::I32(0); 1];
    func.call(&mut store, &[], &mut retval)?;
    println!("{}", retval[0].i32().unwrap());
    Ok(())
}
