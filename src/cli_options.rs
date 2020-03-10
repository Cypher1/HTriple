#[derive(Debug, PartialEq)]
pub struct Options {
    pub files: Vec<String>,
    pub interactive: bool,
    pub wasm: bool,
    pub show_ast: bool,
    pub show_full_ast: bool,
    pub debug: i32,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            files: vec![],
            interactive: false,
            wasm: false,
            show_ast: false,
            show_full_ast: false,
            debug: 0,
        }
    }
}

pub fn parse_args<I, T>(args: I) -> Options
where
    I: IntoIterator<Item = T>,
    T: Into<String>,
{
    let mut opts = Options::default();
    for f in args.into_iter().map(Into::into) {
        if f.is_empty() {
        } else if !f.starts_with('-') {
            opts.files.push(f.to_string());
        } else {
            match f.as_str() {
                "-i" | "--interactive" => {
                    opts.interactive = true;
                    opts.files.push("/dev/stdin".to_string());
                }
                "-r" | "--run" => opts.interactive = true,
                "-d" => opts.debug += 1,
                "--ast" => opts.show_ast = true,
                "--wasm" => opts.wasm = true,
                "--full-ast" => opts.show_full_ast = true,
                "--version" => {
                    println!("{}{}", TITLE, VERSION);
                    return opts;
                }
                arg => {
                    if arg != "-h" && arg != "--help" {
                        eprintln!("unexpected flag '{}'", f);
                    }
                    eprintln!("{}{}\n{}", TITLE, VERSION, USAGE);
                    return opts;
                }
            }
        }
    }
    opts
}

pub const TITLE: &str = "tako v";

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub const USAGE: &str = "An experimental programming language for ergonomic software verification.

Usage:
  tako [-i|-r] [-d <level>] [--ast] [--full-ast] <files>...
  tako (-h | --help)
  tako --version

Options:
  -i --interactive    Run as a repl (interactive mode).
  -r --run            Run files in interpreter.
  -d --debug=<level>  Level of debug logging to use [default: 0].
  --wasm              Compile to wasm [default: false].
  --ast               Pretty print an abstract syntax tree of the code.
  --full_ast          Debug print an abstract syntax tree of the code.
  -h --help           Show this screen.
  --version           Show compiler version.
";

