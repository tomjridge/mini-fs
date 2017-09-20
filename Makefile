all:
	ocamlc -c `ocamldep -sort -one-line *.ml`
