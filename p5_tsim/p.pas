{ 
   Fichero: p.pas                           
   Practica 5: Disenyo e implementacion de una Tabla
               de Simbolos para la manipulacion de 
               bloques en el compilador de Pascal-

   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      20/11/2009                         
}

program p3_stop;

uses analex, syntax, symtable, io, options, errors, ambito, stats;

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

   { Inicializamos variables de analisis }
   line_num := 1; { Inicializamos el contador de linea  }
   col_num := 0;  { Inicializamos contador de columna   }
   i := 1;        { Nos ponemos al principio del buffer }
   Index := 7;    { Inicializamos el contador de IDs    }      
   new(atrib);
   { exit := FALSE; }
    
   { Comenzamos el analisis } 
   init_stats(); { Inicializamos las estadisticas }
   lookahead := yylex(atrib);
   Stop := [ENDTEXTX];
   StandardBlock();
   Program1(Stop);
   finish_stats(); { Finalizamos las estadisticas }
   writeln('   (II) Programa finalizado.');

   { Guardamos estadisticas en fichero }
   if(stat = true) then
      write_stats_file(stat_file);
        
   { Liberamos memoria dinamica } 
   dispose(atrib);
   freemem(buffer, size);
end.
