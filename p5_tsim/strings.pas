{ 
   Fichero: strings.pas                           
   Posee funciones para testear un byte y         
   comprobar si es un char valido asi como una    
   funcion (strtolower) para convertir un string  
   a minusculas.                                  
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      20/11/2009                         
}

unit strings;

interface
   
   { Funciones }
      function isnum(c : char) : boolean;
      function ischar(c : char) : boolean;
      function tolower(c : char) : char;
      procedure strtolower(var str : string);

implementation

   {* Funcion isnum 
    * Devuelve TRUE si el caracter que se le pasa es un numero en ASCII
    * Entrada: caracter a analizar
    * Salida: retorna TRUE si es un numero ASCII 
    *}
   function isnum(c : char) : boolean;
   begin
      if((ord(c) > 47) and (ord(c) < 58)) then
         isnum := true
      else
         isnum := false;
   end;

   {* Funcion ischar
    * Devuelve TRUE si el caracter que se le pasa es una letra ASCII 
      valida
    * Entrada: caracter a analizar
    * Salida: retorna TRUE si es una letra ASCII 
    *}
   function ischar(c : char) : boolean;
   begin
      if(((ord(c) > 64) and (ord(c) < 91)) or ((ord(c) > 96) and (ord(c) < 123))) then
         ischar := true
      else
         ischar := false;
   end;
   
   {* Funcion tolower
    * Convierte un caracter en mayus a uno en minus, si no es una mayus no hace nada
    * Entrada: char en mayus
    * Salida: char en minus
    *}
   function tolower(c : char) : char;
   begin
      tolower := c;
      if((ord(c) > 64) and (ord(c) < 91)) then
         tolower := chr(ord(c) + 32);
   end;

   {* Procedimiento strtolower
    * Convierte una cadena a minusculas
    * Entrada: string que queremos pasar a minus
    * Salida: devuelve cadena en minus 
    *}
   procedure strtolower(var str : string);
   var j : longint;
   begin
      for j := 1 to length(str) do
         str[j] := tolower(str[j]); 
   end;

begin

end.
