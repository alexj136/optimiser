MODULES  = Lexer Parser Syntax Main

OBJECTS  = $(MODULES:=.o)
HIFILES  = $(MODULES:=.hi)

Main: $(MODULES:=.hs) Main.hs
	@ghc --make Main.hs -o Main
	@runhaskell RunTests.hs

Lexer.hs: Lexer.x
	@echo "Generating Lexer"
	@alex Lexer.x

Parser.hs: Parser.y
	@echo "Generating Parser"
	@happy Parser.y

clean:
	@rm -f Main $(OBJECTS) $(HIFILES)
	@echo "Cleaned"