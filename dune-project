(lang dune 3.20)

(name mal)

(generate_opam_files true)

(source
 (github ratakor/mal))

(authors "Ratakor <ratakor@disroot.org>")

(maintainers "Ratakor <ratakor@disroot.org>")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name mal)
 (synopsis "Make a Lisp")
 ; (description "A longer description")
 (depends ocaml containers))
