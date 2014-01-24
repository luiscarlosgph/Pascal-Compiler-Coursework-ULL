{  
   Fichero: analex.pas                            
   Analizador lexico para Pascal-                
   En cada llamada a yylex devuelve un Token      
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      20/11/2009                         
}

unit analex;

interface
   uses symtable, io, errors;

   const
      MAXLEX = 64;
      ENDTEXT = 3;
      MAXTOKSTR = 43;
      TOKSTRLEN = 16;

   type
      Token = (ANDX, ARRAYX, ASTERISKX, BECOMESX, BEGINX, COLONX, 
               COMMAX, CONSTX, DIVX, DOX, DOUBLEDOTX, ELSEX, ENDX, 
               ENDTEXTX, EQUALX, GREATERX, IDX, IFX, LEFTBRACKETX, 
               LEFTPARENTHESISX, LESSX, MINUSX, MODX, NOTX, NOTEQUALX, 
               NOTGREATERX, NOTLESSX, NUMERALX, OFX, ORX, PERIODX, PLUSX, 
               PROCEDUREX, PROGRAMX, RECORDX, RIGHTBRACKETX, 
               RIGHTPARENTHESISX, SEMICOLONX, THENX, TYPEX, TOKEN_ERRORX, 
               VARX, WHILEX);
      
      Tfich = file of char;            { Fichero como array de char }
      TArr = array[1..MAXINT] of char; { Array de char con indices 
                                         ficticios, se podria poner 
                                         array of char pero al gpc no le 
                                         gusta. }
      TArrPtr = ^TArr;                 { Puntero a array de char }
   
   var
      atrib    : argtypePtr; { Atributo de los IDs o NUMERAL }
      TOKEN_STR : array[1..MAXTOKSTR] of string[TOKSTRLEN] = ('AND', 
                  'ARRAY', 'ASTERISK', 'BECOMES', 'BEGIN', 'COLON', 
                  'COMMA', 'CONST', 'DIV', 'DO', 'DOUBLEDOT', 'ELSE', 
                  'END', 'ENDTEXT', 'EQUAL', 'GREATER', 'ID', 'IF', 
                  'LEFTBRACKET', 'LEFTPARENTHESIS', 'LESS', 'MINUS', 
                  'MOD', 'NOT', 'NOTEQUAL', 'NOTGREATER', 'NOTLESS', 
                  'NUMERAL', 'OF', 'OR', 'PERIOD', 'PLUS', 'PROCEDURE', 
                  'PROGRAM', 'RECORD', 'RIGHTBRACKET', 'RIGHTPARENTHESIS',
                  'SEMICOLON', 'THEN', 'TYPE', 'TOKEN_ERROR', 'VAR', 
                  'WHILE');

      Index    : word;      { Indice que va en la estructura de cada elemento de
                              la tabla de simbolos, sera un contador en el caso
                              de los identificadores, y, antes al insertar palabras 
                              reservadas sera el token que les corresponde }

   { Funciones }   
   function yylex(argvalue : argtypePtr) : Token;

implementation

   uses comentarios, strings;

   {* Funcion yylex
    * Devuelve el siguiente token
    * Entrada: 
    *          argvalue -> puntero a los posibles atributos
    *          i        -> posicion por donde vamos leyendo en el buffer
    *          size     -> tamanyo en bytes del buffer del fichero fuente 
    *          buffer   -> array de bytes del fichero fuente
    *          line_num -> numero de linea actual
    *          col_num  -> numero de columna actual
    *          ch       -> caracter leido
    *          tabla    -> tabla de simbolos
    *          Index    -> numero contador de identificadores
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
      ec : integer; { Variable que guarda el error de entero fuera de rango }
   begin 
      { Ignoramos los comentarios }
      Comment();

      { Leemos un caracter }
      c := nextchar();  

      { Ignoramos los espacios, tabuladores, CRLF }
      while((c = chr(SPACE)) or (c = chr(TAB)) or (c = chr(CR)) or (c = chr(LF))) do 
      begin
         if(c = chr(LF)) then
         begin
            col_num := 0;
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
         col_num := 0;
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
               while((isnum(c)) and (k <= 10)) do 
               { Lo de <= 10 es porque el entero mas grande es de 10 cifras }
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
                  write_error(LEXICAL, 'Entero fuera del rango permitido');
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
               Search(lex, isname, ptr, Index);   
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

end.
