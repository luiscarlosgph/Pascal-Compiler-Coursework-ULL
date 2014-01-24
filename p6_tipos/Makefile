COMPILER = fpc -g
SOURCES = analex.pas comentarios.pas \
          io.pas options.pas strings.pas \
			 symtable.pas syntax.pas   

p3_stop: $(SOURCES) 
	$(COMPILER) $(SOURCES) p.pas

clean:
	rm *.ppu
	rm *.o
	rm p
