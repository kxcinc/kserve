(lang dune 2.2)
(name kserve)

(source (github kxcteam/kserve))
(license MIT)
(authors "Kotoi-Xie Consultancy and Haochen Xie")
(maintainers "Haochen Xie <hx@kxc.io>")

(generate_opam_files true)
(package
 (name kserve)
 (synopsis "An http server to ease webapp development and deployment")
 (depends
  (dune (> 2.2))
  (ptime (>= 0.8))
  (digestif (>= 0.8))))
