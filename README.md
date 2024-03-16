# ffbar

(Yet another) ffmpeg wrapper that displays progress on a friendly way.

## Usage

- Piping stderr and stdin
  ```
  $ ffmpeg -nostdin -stats -progress - ... 2>&1 | ffbar
  ```

- As a wrapper
  ```
  $ ffbar ...
  ```
