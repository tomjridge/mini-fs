all:
	for f in minifs.ml in_mem.ml mini_unix.ml; do ocamlc -c $$f; done
