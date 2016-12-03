################################################################################
#                   MAKEFILE POUR LE COMPILATEUR MINI-ADA                      #
#                   Camille Gobert, Anatone Dahan    2016                      #
################################################################################

# Drapeaux pour ocamlc, menhir, ocamllex
CC_FLAGS 	 =
PARSER_FLAGS = --infer
LEXER_FLAGS  =

# Bibliothèques requises
LIBS = nums.cma

# Fichiers objets (devant tous être liés)
# OBJS = ast.cmo lexer.cmo parser.cmo newtyper.cmo compiler.cmo
OBJS = ast.cmo lexer.cmo parser.cmo compiler.cmo

# Cibles factices
.PHONY: clean

all: adac

adac: $(OBJS)
	ocamlc $(CC_FLAGS) $(LIBS) $(OBJS) -o adac

compiler: parser.mli lexer.mli
	ocamlc $(CC_FLAGS) parser.mli lexer.mli -c compiler.ml

newtyper: ast.cmo
#	ocamlc $(CC_FLAGS) ast.mli -c newtyper.ml

parser.cmo: parser.ml ast.cmo
	ocamlc $(CC_FLAGS) -c parser.ml

lexer.cmo: parser.ml lexer.ml
	ocamlc $(CC_FLAGS) parser.mli -c lexer.ml

lexer.ml: parser.ml
	ocamllex $(LEXER_FLAGS) lexer.mll

parser.ml:
	menhir $(PARSER_FLAGS) parser.mly	

ast.cmo:
	ocamlc $(CC_FLAGS) -w -20 -c ast.ml

clean:
	- rm -f adac
	- rm -f *.mli *.cmi *.cmo
