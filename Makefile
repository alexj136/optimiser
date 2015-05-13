main: Lexer.hs Parser.hs Syntax.hs Main.hs
	ghc Main -o main

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y

clean:
	rm -f main *.o *.hi Lexer.hs Parser.hs
