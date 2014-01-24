{ ************************************************** 
  * Practica 2: Analizador sintactico descendente  *
  *             recursivo predictivo para Pascal-  *
  * Asignatura: Compiladores (ULL)                 *
  * Autor:      Luis Carlos Garcia-Peraza Herrera  *
  * Fecha:      18/10/2009                         *
  **************************************************
}

program p2_sintax;

uses analex, syntax, symtable, io;

{ const }

{ type }

var
   fich     : Tfich;     { Manejador del fichero fuente }
   letra    : char;      { Cada caracter leido del fichero }
   j        : word;      { Contador para inicializar la tabla de simbolos }
   { tok      : Token;}  { Token recibido }
   { exit     : boolean;}{ Booleano para parar de leer tokens }

begin
   if((paramcount > 0) and (existe(paramstr(1)))) then
   begin
      { Cargamos el fichero en el buffer }
      assign(fich, paramstr(1));
      reset(fich);
      size := filesize(fich);
      getmem(buffer, size + 1); { Lo de + 1 es para meterle el ENDTEXT } 
      i := 1;
      while not eof(fich) do
      begin
         read(fich, letra); 
         buffer^[i] := letra;
         inc(i);
      end;
      close(fich);
      { Le metemos el ENDTEXT }
      buffer^[size + 1] := char(ENDTEXT);
      
      { Inicializamos la tabla de simbolos } 
      for j := 1 to MAX_KEY do
         tabla[j] := NIL;
      Initialize();

      { Lectura de tokens }
      line_num := 1; { Inicializamos el contador de linea  }
      col_num := 0;  { Inicializamos contador de columna   }
      i := 1;        { Nos ponemos al principio del buffer }
      Index := 7;    { Inicializamos el contador de IDs    }      
      new(atrib);
      { exit := FALSE; }
      
      { Comenzamos el analisis } 
      lookahead := yylex(atrib);
      Program_();
      writeln('   Código correcto.');
      { 
      writeln('LINEA    COLUMNA     TOKEN'); 
      while(exit <> TRUE) do 
      begin
         tok := yylex(atrib); 
         if(tok = ENDTEXTX) then
            exit := TRUE;
         if(tok = IDX) then
            inc(Index);
         Codigo que imprime por pantalla los tokens 
         if(tok = IDX) then
         begin
            inc(Index);
            writeln(line_num, '    ', col_num, '          <', TOKEN_STR[word(tok) + 1], ',', 
                    atrib^.ptr^.lexeme, '>   '); 
         end
         else if(tok = NUMERALX) then
         begin
            writeln(line_num, '    ', col_num, '          <', TOKEN_STR[word(tok) + 1], ',', 
                    atrib^.value, '>   ');
         end
         else
            writeln(line_num, '    ', col_num, '          <', TOKEN_STR[word(tok) + 1], '>   ');
      end;
      }
        
      { Liberamos memoria dinamica } 
      dispose(atrib);
      freemem(buffer, size);
   end
   else
      writeln('   (EE) Introduce un fichero de entrada valido.');
end.
