OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc 
OBJLANG=lambda
SOURCE=lambdaTest
INTERACTIVE_EXE=LambdaTest

UNITTEST_SOURCE=unit_test
UNITTEST_INTERACTIVE_EXE=UnitTest
RM=rm

all: $(INTERACTIVE_EXE) $(UNITTEST_INTERACTIVE_EXE)

#$(INTERACTIVE_EXE): utils.cmo lambda.cmo lambda_parse.cmo lambda_lex.cmo lambdaChecker.cmo $(SOURCE).ml
$(INTERACTIVE_EXE): lambda.cmo lambda_parse.cmo lambda_lex.cmo match_rule.cmo $(SOURCE).ml
	$(OCAMLC) -c $(SOURCE).ml
	$(OCAMLC) -o $(INTERACTIVE_EXE) lambda.cmo lambda_parse.cmo lambda_lex.cmo match_rule.cmo $(SOURCE).cmo 

$(UNITTEST_INTERACTIVE_EXE): lambda.cmo lambda_parse.cmo lambda_lex.cmo match_rule.cmo $(UNITTEST_SOURCE).ml
	$(OCAMLC) -c $(UNITTEST_SOURCE).ml
	$(OCAMLC) -o $(UNITTEST_INTERACTIVE_EXE) lambda.cmo lambda_parse.cmo lambda_lex.cmo match_rule.cmo $(UNITTEST_SOURCE).cmo 

lambda_parse.cmo: lambda_parse.mly lambda.cmo 
	$(OCAMLYACC) lambda_parse.mly
	$(OCAMLC) -c lambda_parse.mli
	$(OCAMLC) -c lambda_parse.ml

lambda_lex.cmo: lambda_lex.mll lambda_parse.cmo
	$(OCAMLLEX) lambda_lex.mll
	$(OCAMLC) -c lambda_lex.ml

lambda.cmo: lambda.ml
	$(OCAMLC) -c lambda.ml

match_rule.cmo: match_rule.ml
	$(OCAMLC) -c match_rule.ml

clean:
	$(RM) *.cm? lambda_lex.ml lambda_parse.ml lambda_parse.mli LambdaTest UnitTest
			

