(lang dune 2.0)
(name tjr_minifs)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/mini-fs))
(homepage             https://github.com/tomjridge/mini-fs)
(documentation        https://tomjridge.github.io/mini-fs)
(generate_opam_files  true)


(package 
 (name tjr_minifs)
 (synopsis "A component-ized filesystem library (FUSE, NFS etc)")
 (depends 
   ocamlfuse
   ppx_jane
   ppx_deriving_yojson
   extunix
   base
   lwt
   lwt.unix
   tjr_net
   tjr_path_resolution))




