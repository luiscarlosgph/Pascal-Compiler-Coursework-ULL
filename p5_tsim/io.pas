{ 
   Fichero: io.pas                                
   Modulo I/O                                     
   Comprueba si existe el fichero que se manda a  
   analizar por linea de comandos y posee las     
   funciones nextchar() y retchar() para leer     
   caracteres del buffer de entrada.              
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      20/11/2009                         
}

unit io;

interface

   type
      Tfich = file of char;            { Fichero como array de char }
      TArr = array[1..MAXINT] of char; { Array de char con indices ficticios, 
                                         se podria poner array of char pero
                                         al gpc no le gusta. }
      TArrPtr = ^TArr;                 { Puntero a array de char }

   var
      source_file : string; { Nombre del fichero fuente }
      stat : boolean;       { Se pone a true si hay que guardar stats }
      stat_file : string;   { Nombre del fichero de las estadisticas }
      buffer   : TArrPtr;   { Buffer donde se guarda el fichero } 
      line_num : word;      { Numero de linea actual }
      col_num  : word;      { Numero de columna actual }
      size     : word;      { Tamanyo del fichero fuente en bytes }
      i        : word;      { Contador para leer fichero }
      ch       : char;      { Caracter analizado actualmente } 

   { Funciones }
      function existe(file_path : string) : boolean;
      function nextchar() : char;
      procedure retchar();

implementation

   uses strings;

   {* Funcion existe
    * Comprueba si un archivo existe.
    * Funcionamiento: desactiva la directiva I y le hace un reset y despues comprueba
    *                 si hubo errores con la funcion IOResult si es distinta de cero 
    *                 es que fallo el reset lo que significa que el archivo no existe.
    *
    * Entrada: file_path es la ruta del archivo para comprobar si existe.
    * Salida : devuelve TRUE si el archivo buscado existe.
    *}
   function existe(file_path : string) : boolean;
   var
      archiv : Tfich;

   begin
      existe := TRUE;
      assign(archiv, file_path);
      {$I-};
      reset(archiv);
      {$I+};
      if(IOResult <> 0) then
         existe := FALSE;
      close(archiv);
   end;

   {* Funcion nextchar
    * Devuelve un nuevo caracter, lo almacena en el atributo ch
    * y avanza a la siguiente posicion del fichero de entrada
    * Entrada: ch      -> caracter leido actualmente
    *          buffer  -> buffer de lectura del fichero fuente
    *          i       -> indice del caracter leido
    *          col_num -> numero de columna del fichero fuente
    * Salida : devuelve el siguiente caracter del fichero de entrada           
    *}
   function nextchar() : char;
   begin
      ch := buffer^[i];
      nextchar := ch;
      inc(i);
      inc(col_num);
   end;

   {* Procedimiento retchar
    * Devuelve un carcater a la entrada porque lo hemos cogido de mas
    * Entrada: i       -> indice del buffer donde estamos
    *          col_num -> numero de columna en el fichero
    *}
   procedure retchar();
   begin
      dec(i);
      dec(col_num);
   end;

begin

end.
