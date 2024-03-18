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
- Or build it yourself (needs a working opam/dune setup).
  ```
  $ make deps
  $ make install
  ```
