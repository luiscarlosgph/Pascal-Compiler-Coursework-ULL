{ 
   Fichero: comentarios.pas                       
   Modulo con la funcion para saltar los          
   comentarios de un codigo fuente Pascal-        
   Soporta comentarios anidados.                  
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      20/11/2009                         
}

unit comentarios;

interface
   uses io, errors;

   const
      CR = 13;
      LF = 10;
      SPACE = 32;
      TAB = 9;
   
   type
      Tfich = file of char;            { Fichero como array de char }
      TArr = array[1..MAXINT] of char; { Array de char con indices ficticios, 
                                         se podria poner array of char pero
                                         al gpc no le gusta. }
      TArrPtr = ^TArr;                 { Puntero a array de char }

   { Procedimientos }
      procedure Comment();

implementation

   {* Procedimiento Comment
    * Ignorara los comentarios
    * Entrada: utiliza la variable global i que sirve para recorrer el buffer, 
               el buffer en si y la variable size que guarda el tamanyo total 
               del buffer.
    *}
   procedure Comment();
   var
      j : word; { Contara los comentarios con llave }
      k : word; { Contara los comentarios con (*    }
   begin
      while((i <= size) and ((buffer^[i] = chr(SPACE)) or (buffer^[i] = chr(TAB)) or 
                             (buffer^[i] = chr(CR)) or (buffer^[i] = chr(LF)))) do 
      begin
         if(buffer^[i] = chr(LF)) then
         begin
            col_num := 0;
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
                        inc(i); { Para el caso llave********llave que no quede 
                                  en bucle infinito }
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
            begin
               if(buffer^[i] = chr(LF)) then
               begin
                  col_num := 0;
                  inc(line_num);
               end
               else
                  inc(col_num);
               inc(i);
            end;
         end;
         if((j = 0) and (k = 0)) then
         begin
            { Ignoramos los espacios, tabuladores, CRLF }
            while((i <= size) and ((buffer^[i] = chr(SPACE)) or 
                                   (buffer^[i] = chr(TAB)) or 
                                   (buffer^[i] = chr(CR)) or 
                                   (buffer^[i] = chr(LF)))) do 
            begin
               if(buffer^[i] = chr(LF)) then
               begin
                  col_num := 0;
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
            else if ((buffer^[i] = '(') and (i <= size - 1) and 
                                            (buffer^[i + 1] = '*')) then
            begin
               inc(k);
               i := i + 2;
            end;
         end;
      end; { Fin while }
      if((j <> 0) or (k <> 0)) then
         write_error(LEXICAL, 'Comentarios sin cerrar adecuadamente, se ha llegado a EOF dentro de un comentario');
   end;

begin

end.
