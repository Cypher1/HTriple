#![deny(clippy::all)]

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

#[macro_use]
mod map_macros;

mod ast;
mod location;
mod parser;
mod tokens;
mod tree;
mod types;

mod cli_options;
mod errors;
mod interpreter;
mod pretty_print;
mod rescoper;
mod to_c;

// The following are only for tests
#[cfg(test)]
mod test_options;
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

use ast::Visitor;
use interpreter::Interpreter;
use pretty_print::PrettyPrint;
use rescoper::ReScoper;

use cli_options::parse_args;
use cli_options::Options;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let opts = parse_args(&args[1..]);
    for f in opts.files.iter() {
        work(&f, &opts)?
    }
    Ok(())
}

fn work(filename: &str, opts: &Options) -> std::io::Result<()> {
    let mut contents = String::new();
    let mut file = File::open(filename.to_string())?;

    file.read_to_string(&mut contents)?;

    let program = parser::parse_file(filename.to_string(), contents);

    let scoped = ReScoper::process(&program, opts).expect("failed scoping");
    if opts.show_full_ast {
        eprintln!("debug ast: {:#?}", scoped);
    }
    if opts.show_ast {
        eprintln!("ast: {}", scoped);
    }

    if opts.interactive {
        use ast::Root;
        use ast::ToNode;
        let res = Interpreter::process(&scoped, opts).expect("could not interpret program");
        let res = PrettyPrint::process(&Root::new(res.to_node()), opts);
        eprintln!(">> {:#?}", res);
        return Ok(());
    }

    let (res, flags) = to_c::Compiler::process(&scoped, opts).expect("could not compile program");

    let start_of_name = filename.rfind('/').unwrap_or(0);
    let dir = &filename[..start_of_name];
    let name = filename.trim_end_matches(".tk");

    std::fs::create_dir_all(format!("build/{}", dir))?;

    let outf = format!("build/{}.cc", name);
    let execf = format!("build/{}", name);
    let destination = std::path::Path::new(&outf);
    let mut f = std::fs::File::create(&destination).expect("could not open output file");
    writeln!(f, "{}", res)?;

    let mut cmd = Command::new("g++");
    for arg in flags.iter() {
        cmd.arg(arg);
    }
    let output = cmd
        .arg("-std=c++14")
        .arg("-Wall")
        .arg("-Werror")
        .arg("-O3")
        .arg(outf)
        .arg("-o")
        .arg(execf)
        .output()?;
    if !output.status.success() {
        let s = String::from_utf8(output.stderr).unwrap();
        eprintln!("{}", s);
        panic!("Command executed with failing error code");
    }
    let s = String::from_utf8(output.stdout).unwrap();
    eprintln!("{}", s);
    Ok(())
}

#[cfg(test)]
mod tests {
    include!(concat!(env!("OUT_DIR"), "/test.rs"));
}
