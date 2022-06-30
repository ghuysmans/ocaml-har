(lang dune 2.0)

(name har)

(generate_opam_files true)

(source
 (github ghuysmans/ocaml-har))

(authors "Guillaume Huysmans <ghuysmans99@gmail.com>")

(maintainers "Guillaume Huysmans <ghuysmans99@gmail.com>")

(license GPL-2.0-or-later)

(package
 (name har)
 (synopsis "HTTP Archive parser and tools")
 (depends ocaml dune base64 ppx_deriving_yojson))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
