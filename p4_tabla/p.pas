{ **************************************************
  * Fichero: p3_stop.pas                           *
  * Practica 3: Analizador sintactico descendente  *
  *             recursivo predictivo con           *
  *             recuperacion de errores para       *
  *             Pascal-                            *
  * Asignatura: Compiladores (ULL)                 *
  * Autor:      Luis Carlos Garcia-Peraza Herrera  *
  * Fecha:      26/10/2009                         *
  **************************************************
}

program p3_stop;

uses analex, syntax, symtable, io, options, errors;

{ const }

{ type }

var
   fich     : Tfich;     { Manejador del fichero fuente }
   letra    : char;      { Cada caracter leido del fichero }
   j        : word;      { Contador para inicializar la tabla de 
                           simbolos }

   { tok      : Token;}  { Token recibido }
   { exit     : boolean;}{ Booleano para parar de leer tokens }

begin
   { Leemos las opciones de la linea de comandos }
   parse_opts();

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

   { Inicializamos las variables de errores }
   last_line_err := 0;

   { Lectura de tokens }
   line_num := 1; { Inicializamos el contador de linea  }
   col_num := 0;  { Inicializamos contador de columna   }
   i := 1;        { Nos ponemos al principio del buffer }
   Index := 7;    { Inicializamos el contador de IDs    }      
   new(atrib);
   { exit := FALSE; }
    
   { Comenzamos el analisis } 
   lookahead := yylex(atrib);
   Stop := [ENDTEXTX];
   Program1(Stop);
   writeln('   (II) Programa finalizado.');
        
   { Liberamos memoria dinamica } 
   dispose(atrib);
   freemem(buffer, size);
end.
