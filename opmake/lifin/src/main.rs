use anyhow::{Result, anyhow, bail};
use clap::Parser;
use std::{
    collections::HashSet,
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

#[derive(Parser)]
#[command(
    name = "lifin",
    about = "Inspect C++ files to guess required libraries"
)]
struct Args {
    /// The public header files to inspect
    #[arg(short = 'p', long)]
    headers: Vec<PathBuf>,
    /// Directory containing source files
    #[arg(short = 'd', long)]
    source_dir: PathBuf,
    /// C++ source files to inspect
    #[arg(short, long)]
    sources: Vec<PathBuf>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
enum IncludeKind {
    System,
    Local,
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct Include {
    kind: IncludeKind,
    header: String,
}

impl Include {
    fn from_line(line: &str) -> Option<Include> {
        let stripped = line.strip_prefix("#include ")?;
        if stripped.len() < 2 {
            return None;
        }
        let kind = if stripped.starts_with('<') && stripped.ends_with('>') {
            IncludeKind::System
        } else if stripped.starts_with('"') && stripped.ends_with('"') {
            IncludeKind::Local
        } else {
            return None;
        };
        Some(Include {
            kind,
            header: stripped[1..stripped.len() - 1].to_string(),
        })
    }
}

fn includes_from_file(path: &PathBuf) -> Result<Vec<Include>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    reader
        .lines()
        .map(|line| Ok(Include::from_line(&line?)))
        .filter_map(Result::transpose)
        .collect()
}

// A library guesser that assumes the first path component of an include is the library name
fn guess_library_from_include(inc: &Include) -> Option<&str> {
    let slash_pos = inc.header.find('/')?;
    let lib_name = &inc.header[..slash_pos];
    Some(lib_name)
}

fn main() -> Result<()> {
    let Args {
        headers,
        source_dir,
        sources,
    } = Args::parse();

    if headers.is_empty() {
        bail!("No header files provided");
    }

    // Ensure header files exist and are absolute paths
    for path in headers.iter() {
        if !path.is_absolute() {
            bail!("Header file {path:?} is not an absolute path");
        }
        if !path.exists() {
            bail!("Header file {path:?} does not exist");
        }
    }

    // Ensure source directory exists and is absolute path
    if !source_dir.exists() {
        bail!("Source directory {source_dir:?} does not exist");
    }
    if !source_dir.is_absolute() {
        bail!("Source directory {source_dir:?} is not an absolute path");
    }

    // Collect source files and make relative paths absolute to source_dir, but leave absolute paths as is
    let abs_sources = sources
        .into_iter()
        .map(|mut path| {
            if !path.is_absolute() {
                path = source_dir.join(path);
            }
            if !path.exists() {
                bail!("Source file {path:?} does not exist");
            }
            Ok(path)
        })
        .collect::<Result<Vec<_>>>()?;

    // Try to find the current library name from public interface
    // Assumes the directory structure is like: /path/to/libname/include/libname/header.h
    let lib_name = headers
        .first()
        .expect("At least one header file is required")
        .components()
        .skip_while(|c| c.as_os_str() != "include")
        .nth(1)
        .and_then(|c| c.as_os_str().to_str())
        .ok_or(anyhow!("Failed to get library name from header path"))?;

    // Parse includes from headers and sources
    let mut hdr_incs = HashSet::new();
    for includes in headers.iter().map(includes_from_file) {
        hdr_incs.extend(includes?);
    }
    if hdr_incs.iter().any(|inc| inc.kind == IncludeKind::Local) {
        bail!("Local includes in public headers are not allowed");
    }

    let mut src_incs = HashSet::new();
    for includes in abs_sources.iter().map(includes_from_file) {
        src_incs.extend(includes?);
    }

    let mut private_libs = src_incs
        .iter()
        .filter(|inc| inc.kind == IncludeKind::System)
        .filter_map(guess_library_from_include)
        .collect::<HashSet<_>>();
    private_libs.remove(lib_name);

    let mut public_libs = hdr_incs
        .iter()
        .filter_map(guess_library_from_include)
        .collect::<HashSet<_>>();
    public_libs.remove(lib_name);

    private_libs.retain(|lib_name| !public_libs.contains(lib_name));

    println!("Inferred library dependencies for '{}':", lib_name);
    if !public_libs.is_empty() {
        println!("  Public dependencies:");
        for lib in public_libs {
            println!("    - {}", lib);
        }
    } else {
        println!("  No public dependencies inferred.");
    }
    if !private_libs.is_empty() {
        println!("  Private dependencies:");
        for lib in private_libs {
            println!("    - {}", lib);
        }
    } else {
        println!("  No private dependencies inferred.");
    }

    Ok(())
}
