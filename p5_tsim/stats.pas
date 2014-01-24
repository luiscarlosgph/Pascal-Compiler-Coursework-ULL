{ 
   Fichero: stats.pas                          
   Tiene las estructuras de datos y 
   los procedimientos relativos a las
   estadisticas de compilacion.
   Datos estadisticos:
      - Numero de colisiones en hash
      - No. identificadores declarados
      - No. ident. referenciados
      - Longitud maxima de las pilas TS
      - Tiempo de compilacion
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      20/11/2009                         
}

unit stats;

interface
   uses io;

   const 
      MAX_LEVEL = 10; { Numero maximo de niveles que se 
                        permite anidar } 

   var
      fh : text;  { Handler del fich. de estaditicas }
      hashcol : integer;  { Colisiones del hash }
      totiden : integer;  { Numero total de identificadores 
                            declarados en el programa }
      totref : integer;   { Numero total de identificadores
                            referenciados en el programa }
      stacklen : integer; { Longitud maxima de las pilas de la TS }
      stacknow : array[1..MAX_LEVEL] of word;
      itime : integer;    { Tiempo inicial de compilacion en ms }
      ftime : integer;    { Tiempo final de compilacion en ms }
      timecom : integer;  { Tiempo de compilacion en milisec } 

   { Funciones }
   procedure init_stats();
   procedure finish_stats();
   procedure write_stats_file(var sf : string);
   
implementation

   { uses sysutils; }

   {* Procedimiento init_stats 
    * Inicializa las variables del modulos de 
    * estadisticas
    *}
   procedure init_stats();
   var
      ihour : word;
      imin : word;
      isec : word;
      imsec : word;
      itemp : TDateTime;
   begin
      { itemp := time();
        decodetime(itemp, ihour, imin, isec, imsec); }
      itime := ihour * 3600 * 1000 + imin * 60 * 1000 + isec * 1000 + imsec;
      hashcol := 0;
      totiden := 0;
      totref := 0;
      stacklen := 0;
   end;

   {* Procedimiento finish_stats
    * Da por terminado el proceso de tomar
    * estadisticas, fundamentalmente es para
    * parar de contar el tiempo de compilacion 
    *}
   procedure finish_stats();
   var
      fhour : word;
      fmin : word;
      fsec : word;
      fmsec : word;
      ftemp : TDateTime;
   begin
      { ftemp := time(); 
        decodetime(itemp, ihour, imin, isec, imsec); }
      ftime := fhour * 3600 * 1000 + fmin * 60 * 1000 + fsec * 1000 + fmsec;
      timecom := ftime - itime;
      totiden := totiden - 7; { Quitamos los identificadores estandar }
   end;

   {* Procedimiento write_stats_file
    * Escribe las estadisticas en el fichero
    * cuyo manejador le pasas como parametro
    *}
   procedure write_stats_file(var sf : string);
   begin
      assign(fh, sf);
      rewrite(fh);
      writeln(fh, '  ____________________________________________ ');
      writeln(fh, ' |   Fichero de estadisticas del compilador   |');
      writeln(fh, ' |   de Pascal-                               |');
      writeln(fh, ' |                                            |');
      writeln(fh, ' |   Alumno: Luis Carlos Garcia-Peraza Herrera|');
      writeln(fh, ' |   alu3793@etsii.ull.es                     |');
      writeln(fh, ' |____________________________________________|');
      writeln(fh);
      writeln(fh);
      writeln(fh, ' FICHERO DE CODIGO FUENTE              -> ', source_file);
      writeln(fh, ' Colisiones de la tabla hash           -> ', hashcol);
      writeln(fh, ' Identificadores declarados            -> ', totiden);
      writeln(fh, ' Identificadores referenciados         -> ', totref);
      writeln(fh, ' Longitud maxima de las pilas en la TS -> ', stacklen);
      { writeln(fh, ' Tiempo de compilacion en milisegundos -> ', timecom); }
      close(fh);     
   end;

begin

end.
