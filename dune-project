(lang dune 3.14)

(name ffbar)

(generate_opam_files true)

(source (github azimut/ffbar))
(authors "azimut")
(maintainers "azimut")
(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ffbar)
 (synopsis "A progress bar for ffmpeg")
 (description "(Yet another) ffmpeg wrapper that displays progress on a friendly way.")
 (depends ocaml dune progress)
 (tags
  (terminal ffmpeg)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
