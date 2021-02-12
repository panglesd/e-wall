(lang dune 2.8)
(name e-wall)

(source (github panglesd/e-wall))
(license GnuGPL)
(authors "Paul-Elliot Anglès d'Auriac")
(maintainers "peada@free.fr")


(package
 (name e-wall)
 (synopsis "An OCaml library ... todo")
 (description "A longer description")
 (depends
  (dune (>= 2.7))
  (containers (>= 3.1))
  (cmdliner (>= 1.0.4))
  (logs (>= 0.7.0))
  (base64 (>= 3.4.0))
  (yojson (>= 1.7.0))
  (ppx_deriving_yojson (>= 3.5.2))
  ; scanner only
  (fileutils (>= 0.6.3))
  (taglib (>= 0.3.6))
  ; library only
  (irmin-unix (>= 2.3.0))
  ; server only
  (opium (>= 0.20.0))
  ; web-app / model only
  (js_of_ocaml-lwt (>= 3.8.0))
   ;  (lwd (>= 0.1))
   ;  (tyxml-lwd (>= 0.1))
   ;(ocamlformat (and :dev (= 0.15.0)))
))

 
