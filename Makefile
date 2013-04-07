SOURCES = exp.ml type.mli type.ml parser.mly lexer.mll main.ml
RESULT = TypingML4
OCAMLMAKEFILE = OCamlMakefile
ANNOTATE = use

include $(OCAMLMAKEFILE)
