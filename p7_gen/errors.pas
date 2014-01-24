{ 
   Fichero: errors.pas                            
   Modulo de gestion de errores                   
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      04/12/2009                         
}

unit errors;

interface
   uses io;

   const
      LEXMSG = 'Lexical';
      SYNMSG = 'Syntax';
      SEMMSG = 'Semantic';

   type
      TError = (LEXICAL, SYNTAX_, SEMANTIC);

   var
      last_line_err : integer; { Linea donde ocurrio el
                                 ultimo error }
      error : boolean = false;

   { Funciones asociadas a este modulo }
   procedure write_error(errt : Terror; msg : string);

implementation
   
   {* Procedimiento write_error
    * Imprime un error del tipo indicado con el mensaje
    * que se le pasa como parametro en msg
    *}
   procedure write_error(errt : Terror; msg : string);
   var
      terrmsg : string;
   begin
      error := true;
      if(line_num <> word(last_line_err)) then
      begin
         case errt of
            LEXICAL:  terrmsg := LEXMSG;
            SYNTAX_:  terrmsg := SYNMSG;
            SEMANTIC: terrmsg := SEMMSG;
         end;
         writeln(source_file, '(', line_num:3, ', ', col_num:3, ') ', terrmsg, ' error: ', msg); 
         last_line_err := line_num;
      end;
   end;

begin

end.
