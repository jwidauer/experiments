#!/bin/env python3

import sys
import argparse
from pathlib import Path


def fail(message: str) -> None:
    print(f"Error: {message}", file=sys.stderr)
    sys.exit(1)


def get_includes_from_file(file_path: Path) -> set[str]:
    """Parse a C++ file to extract included headers up to the first namespace declaration."""

    includes: set[str] = set()

    with file_path.open("r") as f:
        for line in f:
            if line.startswith("namespace"):
                break
            if not line.startswith("#include"):
                continue
            parts = line.split()
            if len(parts) < 2:
                fail(
                    f"Malformed include directive in file {file_path}: '{line.strip()}'"
                )

            include_file = parts[1].strip('"<>')
            includes.add(include_file)

    return includes


def main():
    parser = argparse.ArgumentParser(
        description="Inspect C++ source files to guess required libraries."
    )
    parser.add_argument("headers", nargs="*", help="The public headers to analyze")
    parser.add_argument("sources", nargs="+", help="C++ source files to analyze")
    parser.add_argument("source_dir", help="Directory containing source files")

    args = parser.parse_args()

    # Check that all public headers exist and have absolute paths
    for header in args.headers:
        header = Path(header)
        if not header.is_absolute():
            fail(f"Header '{header}' is not an absolute path.")
        if not header.is_file():
            fail(f"Header '{header}' does not exist.")

    source_dir = Path(args.source_dir)
    if not source_dir.is_dir():
        fail(f"Source directory '{source_dir}' does not exist or is not a directory.")

    # Collect source files and make relative paths absolute to source_dir, but leave absolute paths as is
    sources: list[Path] = []
    for source in args.sources:
        source_path = Path(source)
        if not source_path.is_absolute():
            source_path = source_dir / source_path
        if not source_path.is_file():
            fail(f"Source file '{source_path}' does not exist.")
        sources.append(source_path)

    source_includes: set[str] = set()
    for source in sources:
        source_includes.update(get_includes_from_file(source))

    return 0


if __name__ == "__main__":
    sys.exit(main())
