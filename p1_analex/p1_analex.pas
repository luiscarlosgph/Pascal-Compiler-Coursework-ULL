{ ************************************************** 
  * Practica 1: Analizador lexico para Pascal-     *
  * Asignatura: Compiladores (ULL)                 *
  * Autor:      Luis Carlos Garcia-Peraza Herrera  *
  * Fecha:      30/9/2009                          *
  **************************************************
}

program p1_analex;

const
   INIT    = $811c9dc5;
   PRIME   = $01000193;
   MAX_KEY = 503; 
   PR_SIZE = 20;
   PR : array[1..PR_SIZE] of string[16] = ('and', 'array', 'begin', 'const', 'div',
                                           'do', 'else', 'end', 'if', 'mod',
                                           'not', 'of', 'or', 'procedure', 'program',
                                           'record', 'then', 'type', 'var', 'while');
   PR_TOK : array[1..PR_SIZE] of word = (0, 1, 4, 7, 8, 9, 11, 12, 17, 22, 23, 28, 29, 32, 33, 34, 38, 39, 41, 42);
   IE : array[1..6] of string[16] = ('integer', 'boolean', 'false', 'true', 'read', 'write');
   TOKEN_STR : array[1..43] of string[16] = ('AND', 'ARRAY', 'ASTERISK', 'BECOMES', 'BEGIN', 'COLON', 
                                     'COMMA', 'CONST', 'DIV', 'DO', 'DOUBLEDOT', 'ELSE', 'END',
                                     'ENDTEXT', 'EQUAL', 'GREATER', 'ID', 'IF', 'LEFTBRACKET',
                                     'LEFTPARENTHESIS', 'LESS', 'MINUS', 'MOD', 'NOT', 'NOTEQUAL',
                                     'NOTGREATER', 'NOTLESS', 'NUMERAL', 'OF', 'OR', 'PERIOD', 
                                     'PLUS', 'PROCEDURE', 'PROGRAM', 'RECORD', 'RIGHTBRACKET',
                                     'RIGHTPARENTHESIS', 'SEMICOLON', 'THEN', 'TYPE', 
                                     'TOKEN_ERROR', 'VAR', 'WHILE');
   SPACE = 32;
   TAB = 9;
   CR = 13;
   LF = 10;
   ENDTEXT = 3;
   MAXLEX = 32;

type
   Token = (ANDX, ARRAYX, ASTERISKX, BECOMESX, BEGINX, COLONX, COMMAX, CONSTX, DIVX, DOX,
            DOUBLEDOTX, ELSEX, ENDX, ENDTEXTX, EQUALX, GREATERX, IDX, IFX, LEFTBRACKETX,
            LEFTPARENTHESISX, LESSX, MINUSX, MODX, NOTX, NOTEQUALX, NOTGREATERX, NOTLESSX, 
            NUMERALX, OFX, ORX, PERIODX, PLUSX, PROCEDUREX, PROGRAMX, RECORDX, RIGHTBRACKETX,
            RIGHTPARENTHESISX, SEMICOLONX, THENX, TYPEX, TOKEN_ERRORX, VARX, WHILEX);
   WordPointer = ^WordRecord;
   WordRecord = record { Tabla de simbolos }
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
   Tfich = file of char;            { Fichero como array de char }
   TArr = array[1..MAXINT] of char; { Array de char con indices ficticios, 
                                      se podria poner array of char pero
                                      al gpc no le gusta. }
   TArrPtr = ^TArr;                 { Puntero a array de char }
   TSymArray = array[1..MAX_KEY] of WordPointer;

var
   fich     : Tfich;     { Manejador del fichero fuente }
   size     : word;      { Tamanyo del fichero fuente en bytes }
   buffer   : TArrPtr;   { Buffer donde se guarda el fichero } 
   letra    : char;      { Cada caracter leido del fichero }
   i        : word;      { Contador para leer fichero }
   j        : word;      { Contador para inicializar la tabla de simbolos }
   symtable : TSymArray; { Tabla de simbolos }
   Index    : word;      { Indice que va en la estructura de cada elemento de
                           la tabla de simbolos, sera un contador en el caso
                           de los identificadores, y, antes al insertar palabras 
                           reservadas sera el token que les corresponde }
   line_num : word;      { Numero de linea actual }
   col_num  : word;      { Numero de columna actual }
   ch       : char;      { Caracter analizado actualmente } 
   tok      : Token;     { Token recibido }
   atrib    : argtypePtr; { Atributo de los IDs o NUMERAL }
   exit     : boolean;   { Booleano para parar de leer tokens }

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

   {* Funcion hash
    * Entrada: str -> string
    * Salida : devuelve un word con el valor hash 
    *}
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
    * Salida:  Ptr      -> devuelve un puntero a la estructura del id o PR
    *}
   procedure Insert(IsName : boolean; var WordText : string; Index : word; KeyNo : word; var Ptr : WordPointer);
   var
      p : WordPointer;
   begin
      new(p);    
      p^.NextWord := symtable[KeyNo]; { Nuestro siguiente es el primero de la lista, si no habia nadie pues NIL }
      p^.lexeme := WordText;
      p^.IsName := IsName;
      p^.Index := Index;
      symtable[KeyNo] := p; { Nos metemos los primeros de la lista }
      Ptr := p;
   end;

   {* Procedimiento Initialize
    * Inicializa la tabla de simbolos con las palabras resservadas 
    * y los identificadores estandar
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
    * Salida:  IsName   -> devuelve TRUE si es un id
    *          Ptr      -> devuelve un puntero a la estructura del id o PR
    *}
   procedure Search(var WordText : string; var IsName : boolean; var Ptr : WordPointer);
   var
      hashval : longword;
      p : WordPointer;
   begin
      hashval := hash(WordText);
      p := symtable[hashval];
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
      end;
   end;


   {* Procedimiento Comment
    * Ignorara los comentarios
    * Entrada: utiliza la variable global i que sirve para recorrer el buffer, el buffer en si
               y la variable size que guarda el tamanyo total del buffer.
    *}
   procedure Comment();
   var
      j : word; { Contara los comentarios con llave }
      k : word; { Contara los comentarios con (*    }
   begin
      while((i <= size) and ((buffer^[i] = chr(SPACE)) or (buffer^[i] = chr(TAB)) or (buffer^[i] = chr(CR)) or (buffer^[i] = chr(LF)))) do 
      begin
         if(buffer^[i] = chr(LF)) then
         begin
            col_num := 1;
            inc(line_num);
         end
         else
            inc(col_num);
         inc(i);
      end;

         j := 0;
         k := 0;
         if((buffer^[i] = '(') and (buffer^[i + 1] = '*')) then { Comentario con (* }
         begin
            inc(k); 
            i := i + 2; 
         end
         else if(buffer^[i] = '{') then                         { Comentario con llave }
         begin
            inc(j); 
            inc(i);     
         end;
         while(((j > 0) or (k > 0)) and (i <= size)) do
         begin
            case buffer^[i] of
               '}':  begin
                        dec(j);
                        inc(i);
                     end;
               '*':  begin
                        if(buffer^[i + 1] = ')') then
                        begin
                           dec(k);
                           i := i + 2;
                        end
                        else
                           inc(i); { Para el caso llave********llave que no quede en bucle infinito }
                     end;
               '{':  begin
                        inc(j);
                        inc(i);
                     end;
               '(':  begin
                        if(buffer^[i + 1] = '*') then
                        begin
                           inc(k);
                           i := i + 2;
                        end
                        else
                           inc(i);
                     end;
               else
                  inc(i);
            end;
            if((j = 0) and (k = 0)) then
            begin
               { Ignoramos los espacios, tabuladores, CRLF }
               while((i <= size) and ((buffer^[i] = chr(SPACE)) or (buffer^[i] = chr(TAB)) or (buffer^[i] = chr(CR)) or (buffer^[i] = chr(LF)))) do 
               begin
                  if(buffer^[i] = chr(LF)) then
                  begin
                     col_num := 1;
                     inc(line_num);
                  end
                  else
                     inc(col_num);
                  inc(i);
               end;
               if(buffer^[i] = '{') then
               begin
                  inc(j);
                  inc(i);
               end
               else if ((buffer^[i] = '(') and (i <= size - 1) and (buffer^[i + 1] = '*')) then
               begin
                  inc(k);
                  i := i + 2;
               end;
            end;
         end;
         if((j <> 0) or (k <> 0)) then
            writeln('   (EE) Comentarios sin cerrar adecuadamente. Se ha llegado a EOF dentro de un comentario.');
   end;

   {* Funcion nextchar
    * Devuelve un nuevo caracter, lo almacena en el atributo ch
    * y avanza a la siguiente posicion del fichero de entrada
    * Entrada: 
    * Salida : devuelve el siguiente caracter del fichero de entrada           
    *}
   function nextchar() : char;
   begin
      ch := buffer^[i];
      nextchar := ch;
      inc(i);
   end;

   {* Procedimiento retchar
    * Devuelve un carcater a la entrada porque lo hemos cogido de mas
    *}
   procedure retchar();
   begin
      dec(i);
   end;

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
    * Devuelve TRUE si el caracter que se le pasa es una letra ASCII valida
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

   {* Funcion yylex
    * Devuelve el siguiente token
    * Entrada: 
    * Salida : devuelve un token y si es necesario su atributo           
    *}
   function yylex(argvalue : argtypePtr) : Token;
   var
      c : char;             { Caracter leido }
      aux : char;           { Caracter auxiliar para leer uno mas cuando hace falta }
      lex : string[MAXLEX]; { Lexema }
      k : word;             { Contador }
      isname : boolean;     { Booleano para saber si una cadena es PR o ID }
      ptr : WordPointer;    { Puntero a un ID o PR de la tabla de simbolos }
      ec : integer;
   begin 
      { Ignoramos los comentarios }
      Comment();

      { Leemos un caracter }
      c := nextchar();  
      inc(col_num);

      { Ignoramos los espacios, tabuladores, CRLF }
      while((c = chr(SPACE)) or (c = chr(TAB)) or (c = chr(CR)) or (c = chr(LF))) do 
      begin
         if(c = chr(LF)) then
         begin
            col_num := 1;
            inc(line_num);
         end
         else
            inc(col_num);
         c := nextchar(); 
      end;
      
      { Ignoramos retornos de carro }
      while(c = chr(LF)) do
      begin
         c := nextchar();
         inc(line_num);
         col_num := 1;
      end;
      
      { Analizamos el caracter }
      case c of
         { Operadores }
         '+': yylex := PLUSX;
         '-': yylex := MINUSX;
         '*': yylex := ASTERISKX;
         '=': yylex := EQUALX;
         '>':  begin
                  if(nextchar() = '=') then
                     yylex := NOTLESSX
                  else
                  begin
                     yylex := GREATERX;
                     retchar();
                  end;
               end;
         '<':  begin
                  aux := nextchar();
                  if(aux = '=') then
                     yylex := NOTGREATERX
                  else if(aux = '>') then
                     yylex := NOTEQUALX
                  else
                  begin
                     yylex := LESSX;
                     retchar();
                  end;
               end;
         ':':  begin
                  if(nextchar() = '=') then
                     yylex := BECOMESX
                  else
                  begin
                     yylex := COLONX;
                     retchar();
                  end;
               end;
         '(': yylex := LEFTPARENTHESISX;
         ')': yylex := RIGHTPARENTHESISX;
         '[': yylex := LEFTBRACKETX;
         ']': yylex := RIGHTBRACKETX;
         ',': yylex := COMMAX;
         '.':  begin
                  if(nextchar() = '.') then
                     yylex := DOUBLEDOTX
                  else
                  begin
                     yylex := PERIODX;
                     retchar();
                  end;
               end;
         ';': yylex := SEMICOLONX; 
         chr(ENDTEXT): yylex := ENDTEXTX;
         
         else { Case por defecto }
         begin
            if(isnum(c)) then         { Si es un numero }
            begin
               yylex := NUMERALX;
               k := 1; 
               while((isnum(c)) and (k <= 10)) do { Lo de <= 10 es porque el entero mas grande es de 10 cifras }
               begin
                  lex[k] := c;
                  inc(k);
                  c := nextchar();
               end;
               retchar(); { Devolvemos lo que cogimos de mas }
               setlength(lex, k - 1);
               { Convertimos el numero a entero }
               val(lex, argvalue^.value, ec);
               if(ec <> 0) then
                  writeln('   (EE) Error, entero no valido en la linea ', line_num, ' y columna ', col_num, '.');
            end
            else if(ischar(c)) then   { Si es una letra }
            begin
               k := 1;
               while(((ischar(c)) or (isnum(c)) or (c = '_')) and (k <= MAXLEX)) do
               begin
                  c := tolower(c);
                  lex[k] := c;
                  inc(k);
                  c := nextchar();
               end;
               retchar();
               setlength(lex, k - 1);
               Search(lex, isname, ptr);   
               if(isname = true) then
               begin
                  yylex := IDX;
                  argvalue^.ptr := ptr;
               end
               else
                  yylex := Token(ptr^.Index);
            end
            else
               yylex := TOKEN_ERRORX;
         end { Cierro case else }
      end; { Cierro case }
   end;

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
         symtable[j] := NIL;
      Initialize();

      { Lectura de tokens }
      line_num := 1; { Inicializamos el contador de linea  }
      col_num := 1;  { Inicializamos contador de columna   }
      i := 1;        { Nos ponemos al principio del buffer }
      Index := 7;    { Inicializamos el contador de IDs    }      
      new(atrib);
      exit := FALSE;
      while(exit <> TRUE) do 
      begin
         tok := yylex(atrib); 
         if(tok = ENDTEXTX) then
            exit := TRUE;
         if(tok = IDX) then
         begin
            inc(Index);
            write('<', TOKEN_STR[word(tok) + 1], ',', atrib^.ptr^.lexeme, '>   '); 
         end
         else if(tok = NUMERALX) then
         begin
            write('<', TOKEN_STR[word(tok) + 1], ',', atrib^.value, '>   ');
         end
         else
            write('<', TOKEN_STR[word(tok) + 1], '>   '); 
      end;
      writeln;

      { Liberamos memoria dinamica } 
      dispose(atrib);
      freemem(buffer, size);
   end
   else
      writeln('   (EE) Introduce un fichero de entrada valido.');
end.
