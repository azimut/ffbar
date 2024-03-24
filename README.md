# ffbar

(Yet another) ffmpeg wrapper that displays progress on a friendly way.

## Usage

- As a wrapper
  ```
  $ ffbar -i in.mp4 out.mp4
  ```

- Piping both `stdout` and `stderr`
  ```
  $ ffmpeg -nostdin -stats -progress - -i in.mp4 out.mp4 2>&1 | ffbar
  ```

## Installation

- Download one of the available static compiled binaries from the [releases](https://github.com/azimut/ffbar/releases/) page.
- Or build it yourself (needs a working [opam](https://opam.ocaml.org/doc/Install.html) setup).
  ```
  $ make deps
  $ make install
  ```

## Lines of Code (cloc . --vcs=git --md)

Language|files|blank|comment|code
:-------|-------:|-------:|-------:|-------:
OCaml|1|8|0|79
YAML|2|0|0|48
Dockerfile|1|5|0|19
Markdown|1|6|0|18
make|1|4|0|14
--------|--------|--------|--------|--------
SUM:|6|23|0|178
