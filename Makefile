all:
	for f in minifs.ml in_mem.ml mini_unix.ml; do ocamlfind ocamlc -package extunix -c $$f; done
