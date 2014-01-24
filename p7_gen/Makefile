COMPILER = fpc -g
MAIN = p.pas
SOURCES = analex.pas comentarios.pas \
          io.pas options.pas strings.pas \
			 symtable.pas syntax.pas gencode.pas 

p3_stop: $(SOURCES) 
	$(COMPILER) $(MAIN)

clean:
	rm *.ppu
	rm *.o
	rm p
