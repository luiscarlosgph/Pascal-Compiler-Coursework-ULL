{ 
   Fichero: symtable.pas                          
   Manejo de las estructura de la tabla de        
   simbolos, permite inicializarla, insertar      
   elementos y buscar                             
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      26/10/2009                         
}

unit symtable;

interface
   const
      { INIT    = $811c9dc5; }
      { PRIME   = $01000193; }
      MAX_KEY = 1009;
      MAXLEX = 64;
      PR_SIZE = 20;
      PR : array[1..PR_SIZE] of string[16] = ('and', 'array', 'begin', 
                                              'const', 'div', 'do', 
                                              'else', 'end', 'if', 'mod',
                                              'not', 'of', 'or', 
                                              'procedure', 'program', 
                                              'record', 'then', 'type', 
                                              'var', 'while');
      PR_TOK : array[1..PR_SIZE] of word = (0, 1, 4, 7, 8, 9, 11, 12, 17, 
                                            22, 23, 28, 29, 32, 33, 34, 
                                            38, 39, 41, 42);
      IE : array[1..6] of string[16] = ('integer', 'boolean', 'false', 
                                        'true', 'read', 'write');
      MAX_LEVEL = 10; { Numero maximo de niveles que se permite anidar } 

   type 
      WordPointer = ^WordRecord;
      WordRecord = record { Tipo de dato de tabla de simbolos }
                      NextWord : WordPointer;
                      lexeme   : string[MAXLEX];      
                      IsName   : boolean; { TRUE si es un identificador }
                      Index    : word;
                   end;
      argtype = record
                   case boolean of
                      false: (ptr   : WordPointer);
                      true:  (value : longint);
                   end;
      argtypePtr = ^argtype;
      TSymArray = array[1..MAX_KEY] of WordPointer;

   var
         tabla : TSymArray; { TABLA DE SIMBOLOS }

   { Funciones }
      function hash(var str : string) : longword;
      procedure Insert(IsName : boolean; var WordText : string; Index : word; 
                       KeyNo : word; var Ptr : WordPointer);
      procedure Initialize();
      procedure Search(var WordText : string; var IsName : boolean; var Ptr : WordPointer; var Index : word);

implementation

   uses stats;

   {* Funcion hash
    * Entrada: str -> string
    * Salida : devuelve un word con el valor hash 
    *
   function hash(var str : string) : longword;
   var
      i : longword;
      ret : longword;
   begin
      ret := INIT;
      for i := 1 to length(str) do 
      begin
         ret := ret xor ord(str[i]); 
         ret := ret * PRIME; 
      end;
      ret := ret mod MAX_KEY;
      hash := ret;
   end; }

   {* Funcion hash
    * Entrada: str -> string
    * Salida : devuelve un word con el valor hash 
    *}
   function hash(var str : string) : longword;
   var
      i : longword;
      ret : longword;
   begin
      ret := 0;
      for i := 1 to length(str) do
      begin
         ret := ret + ord(str[i]); 
         ret := ret + (10 shl ret);
         ret := ret xor (6 shr ret);
      end;
      ret := ret mod MAX_KEY; 
      hash := ret;
   end;   

   {* Procedimiento Insert 
    * Inserta en la tabla de simbolos una cadena de caracteres, que
    * puede ser un identificador o una palabra reservada. En el caso
    * de un identificador Index tiene que ser un ordinal y si es una
    * palabra reservada tendra que ser el numero que se corresponde
    * con el token del enumerado.
    * Entrada: IsName   -> es TRUE si se trata de un identificador
    *          WordText -> nombre del identificador
    *          Index    -> indice del id o PR
    *          KeyNo    -> indice de la tabla hash donde va
    *          tabla    -> tabla de simbolos
    * Salida:  Ptr      -> devuelve un puntero a la estructura del id o PR
    *}
   procedure Insert(IsName : boolean; var WordText : string; Index : word; 
                    KeyNo : word; var Ptr : WordPointer);
   var
      p : WordPointer;
   begin
      new(p);    
      p^.NextWord := tabla[KeyNo]; { Nuestro siguiente es el primero de la lista, si no habia nadie pues NIL }
      p^.lexeme := WordText;
      p^.IsName := IsName;
      p^.Index := Index;
      if(tabla[KeyNo] <> NIL) then
         inc(hashcol);
      tabla[KeyNo] := p; { Nos metemos los primeros de la lista }
      Ptr := p;
   end;

   {* Procedimiento Initialize
    * Inicializa la tabla de simbolos con las palabras resservadas 
    * y los identificadores estandar
    * E/S : tabla -> tabla de simbolos
    *}
   procedure Initialize();
   var
      p : WordPointer;
      WordText : string[16];
      i : word;
   begin
      { Insertamos palabras reservadas }
      for i := 1 to PR_SIZE do
      begin
         WordText := PR[i];
         Insert(FALSE, WordText, PR_TOK[i], hash(WordText), p); 
      end; 
      
      { Insertamos identificadores estandar }
      for i := 1 to 6 do
      begin
         WordText := IE[i];
         Insert(TRUE, WordText, i, hash(WordText), p); 
      end;
   end;

   {* Procedimiento Search
    * Calcula el valor de la funcion hash asociado a la cadena WordText
    * Busca WordText en la lista correspondiente a la tabla de simbolos
    * Si encuentra WordText devuelve un puntero al record y un valor
    * boolean que dice si es PR o identificador, sino esta la inserta y
    * devuelve el puntero y el boolean tambien.
    *
    * Entrada: WordText -> lexema a buscar en la TS
    *          tabla    -> tabla de simbolos
    *          Index    -> indice (variable global) del numero de identificador actual
    * Salida:  IsName   -> devuelve TRUE si es un id
    *          Ptr      -> devuelve un puntero a la estructura del id o PR
    *}
   procedure Search(var WordText : string; var IsName : boolean; var Ptr : WordPointer; var Index : word);
   var
      hashval : longword;
      p : WordPointer;
   begin
      hashval := hash(WordText);
      p := tabla[hashval];
      while((p <> NIL) and (p^.lexeme <> WordText)) do
      begin  
         p := p^.NextWord;
      end;
      if(p <> NIL) then { Si se paro en algun elemento, estaba en la tabla }
      begin
         Ptr := p; 
         IsName := p^.IsName;
      end
      else              { No se paro, luego no estaba insertado en la tabla }
      begin
         IsName := TRUE; { Es un identificador porque se supone que las PR ya estan insertadas,
                           no va a pasar que las busquemos y no esten }
         Insert(IsName, WordText, Index, hashval, Ptr); { WARNING, variable global -> Index <- al canto!! }
         { Si piensas que lo que vas a buscar puede que no este, pon el Index global correctamente }
         inc(Index);
      end;
   end;
   
begin

end.
