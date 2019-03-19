default: index.html

index.html: src/Main.elm
	elm make src/Main.elm --output=build/index.html

clean:
	-rm -f *.o *.exe
