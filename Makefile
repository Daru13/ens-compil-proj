################################################################################
#                   MAKEFILE POUR LE COMPILATEUR MINI-ADA                      #
#                   Camille Gobert, Anatone Dahan 2016-17                      #
################################################################################

# Drapeaux pour $(CC), menhir, ocamllex
CC 			 = ocamlc
CC_FLAGS 	 = 
PARSER_FLAGS = --infer
LEXER_FLAGS  =

# Bibliothèques externes requises
LIBS = nums.cma

# Fichiers objets (devant tous être liés)
OBJS = ast.cmo lexer.cmo parser.cmo typer.cmo compiler.cmo x86_64.cmo encoder_x86.cmo
# OBJS = ast.cmo lexer.cmo parser.cmo compiler.cmo

# Cibles factices
.PHONY: clean

# Cibles à compiler et lier
all: adac

adac: $(OBJS)
	$(CC) $(CC_FLAGS) $(LIBS) $(OBJS) -o adac

encoder_x86.cmo: encoder_x86.ml x86_64.cmo ast.cmo typer.cmo
	$(CC) $(CC_FLAGS) -c encoder_x86.ml

x86_64.cmo: x86_64.ml x86_64.cmi
	$(CC) $(CC_FLAGS) -c x86_64.ml

x86_64.cmi: x86_64.mli
	$(CC) $(CC_FLAGS) -c x86_64.mli

compiler.cmo: compiler.ml parser.cmo lexer.cmo
	$(CC) $(CC_FLAGS) parser.mli -c compiler.ml

typer.cmo: typer.ml ast.cmo
	$(CC) $(CC_FLAGS) -c typer.ml

parser.cmo: parser.ml ast.cmo
	$(CC) $(CC_FLAGS) -c parser.ml

lexer.cmo: lexer.ml parser.ml
	$(CC) $(CC_FLAGS) parser.mli -c lexer.ml

lexer.ml: lexer.mll ast.cmo
	ocamllex $(LEXER_FLAGS) lexer.mll

parser.ml: parser.mly
	menhir $(PARSER_FLAGS) parser.mly	

ast.cmo: ast.ml
	$(CC) $(CC_FLAGS) -w -30 -c ast.ml

clean:
	- rm -f adac
	- rm -f lexer.ml parser.ml
	# Solution moche pour ne pas supprimer cette interface
	- mv x86_64.mli .x86
	- rm -f *.mli *.cmi *.cmo
	- mv .x86 x86_64.mli
