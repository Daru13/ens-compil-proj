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
OBJS = ast.cmo lexer.cmo parser.cmo typer.cmo compiler.cmo
# OBJS = ast.cmo lexer.cmo parser.cmo compiler.cmo

# Cibles factices
.PHONY: clean

# Cibles à compiler et lier
all: adac

adac: $(OBJS)
	ocamlc $(CC_FLAGS) $(LIBS) $(OBJS) -o adac

compiler.cmo: compiler.ml parser.cmo lexer.cmo
	ocamlc $(CC_FLAGS) parser.mli -c compiler.ml

typer.cmo: typer.ml ast.cmo
	ocamlc $(CC_FLAGS) -c typer.ml

parser.cmo: parser.ml ast.cmo
	ocamlc $(CC_FLAGS) -c parser.ml

lexer.cmo: lexer.ml parser.ml
	ocamlc $(CC_FLAGS) parser.mli -c lexer.ml

lexer.ml: lexer.mll ast.cmo
	ocamllex $(LEXER_FLAGS) lexer.mll

parser.ml: parser.mly
	menhir $(PARSER_FLAGS) parser.mly	

ast.cmo: ast.ml
	ocamlc $(CC_FLAGS) -w -30 -c ast.ml

clean:
	- rm -f adac
	- rm -f lexer.ml parser.ml
	- rm -f *.mli *.cmi *.cmo
