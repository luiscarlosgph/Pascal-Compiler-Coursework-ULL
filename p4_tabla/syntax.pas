{ 
  Fichero: syntax.pas                            
  Modulo del analizador sintactico               
  descendente predictivo no recursivo 
 
  Asignatura: Compiladores (ULL)                 
  Autor:      Luis Carlos Garcia-Peraza Herrera  
  Fecha:      10/11/2009                         
}

unit syntax;

interface
   uses analex, io, errors;

   type
      { Estos son todos los simbolos gramaticales con los que
        construiremos las reglas }
      TSymGram = (ANDX, ARRAYX, ASTERISKX, BECOMESX, BEGINX, COLONX,
         COMMAX, CONSTX, DIVX, DOX, DOUBLEDOTX, ELSEX, ENDX,
         ENDTEXTX, EQUALX, GREATERX, IDX, IFX, LEFTBRACKETX,
         LEFTPARENTHESISX, LESSX, MINUSX, MODX, NOTX, NOTEQUALX,
         NOTGREATERX, NOTLESSX, NUMERALX, OFX, ORX, PERIODX, PLUSX,
         PROCEDUREX, PROGRAMX, RECORDX, RIGHTBRACKETX,
         RIGHTPARENTHESISX, SEMICOLONX, THENX, TYPEX, EPSILON, 
         { OJO! EPSILON se corresponde con TOKEN_ERROR ^^^^ }
         VARX, WHILEX, 
         { A partir de aqui empiezan los no terminales... }
         PROGRAM1, BLOCK_BODY, BLOCK_BODY2, BLOCK_BODY3, 
         BLOCK_BODY4, BLOCK_BODY5, CONSTANT_DEFINITION_PART, 
         CONSTANT_DEFINITION_PART2, CONSTANT_DEFINITION, 
         TYPE_DEFINITION_PART, TYPE_DEFINITION_PART2, TYPE_DEFINITION,
         NEW_TYPE, NEW_ARRAY_TYPE, INDEX_RANGE, NEW_RECORD_TYPE, 
         FIELD_LIST, FIELD_LIST2, RECORD_SECTION, RECORD_SECTION2, 
         VARIABLE_DEFINITION_PART, VARIABLE_DEFINITION_PART2, 
         VARIABLE_DEFINITION, VARIABLE_GROUP, VARIABLE_GROUP2, 
         PROCEDURE_DEFINITION, PROCEDURE_BLOCK, PROCEDURE_BLOCK2,
         FORMAL_PARAMETER_LIST, FORMAL_PARAMETER_LIST2, 
         PARAMETER_DEFINITION, PARAMETER_DEFINITION2, STATEMENT,
         STATEMENT2, STATEMENT3, PROCEDURE_STATEMENT, 
         ACTUAL_PARAMETER_LIST, ACTUAL_PARAMETER_LIST2, IF_STATEMENT,
         IF_STATEMENT2, WHILE_STATEMENT, COMPOUND_STATEMENT,
         COMPOUND_STATEMENT2, EXPRESSION, EXPRESSION2,
         RELATIONAL_OPERATOR, SIMPLE_EXPRESSION, SIMPLE_EXPRESSION2,
         SIMPLE_EXPRESSION3, SIGN_OPERATOR, ADDITIVE_OPERATOR, TERM,
         TERM2, MULTIPLYING_OPERATOR, FACTOR, FACTOR2, SELECTOR,
         INDEX_SELECTOR, FIELD_SELECTOR, CONSTANT, NULL = -1); 
                                  { OJO! Warning al poner -1 }
      { Le ponemos nombre a las partes derechas
        de las reglas, para que sea algo mas personal }
      { Indice de reglas }
      RULENAMES = (PROGRAM_R = 1, 
         BLOCK_BODY_R = 6, 
         BLOCK_BODY2_R = 11,
         EPSILON_R = 12, { OJO! Solo hay una regla para EPSILON } 
         BLOCK_BODY3_R = 13, 
         BLOCK_BODY4_R = 14, 
         BLOCK_BODY5_R = 15, 
         CONSTANT_DEFINITION_PART_R = 17, 
         CONSTANT_DEFINITION_PART2_R = 20,
         CONSTANT_DEFINITION_R = 22, 
         TYPE_DEFINITION_PART_R = 26,
         TYPE_DEFINITION_PART2_R = 29, 
         TYPE_DEFINITION_R = 31,
         NEW_TYPE_R = 35, 
         NEW_TYPE_R2 = 36, 
         NEW_ARRAY_TYPE_R = 37, 
         INDEX_RANGE_R = 43, 
         NEW_RECORD_TYPE_R = 46, 
         FIELD_LIST_R = 49, 
         FIELD_LIST2_R = 51,
         RECORD_SECTION_R = 54, 
         RECORD_SECTION2_R = 58,
         VARIABLE_DEFINITION_PART_R = 61, 
         VARIABLE_DEFINITION_PART2_R = 64,
         VARIABLE_DEFINITION_R = 66, 
         VARIABLE_GROUP_R = 68, 
         VARIABLE_GROUP2_R = 72,
         PROCEDURE_DEFINITION_R = 75, 
         PROCEDURE_BLOCK_R = 79, 
         PROCEDURE_BLOCK2_R = 82, 
         FORMAL_PARAMETER_LIST_R = 85, 
         FORMAL_PARAMETER_LIST2_R = 87, 
         PARAMETER_DEFINITION_R = 90,
         PARAMETER_DEFINITION2_R = 92, 
         STATEMENT_R = 93, 
         STATEMENT_R2 = 95, 
         STATEMENT_R3 = 96, 
         STATEMENT_R4 = 97, 
         STATEMENT2_R = 98, 
         STATEMENT2_R2 = 99, 
         STATEMENT3_R = 102, 
         PROCEDURE_STATEMENT_R = 104, 
         ACTUAL_PARAMETER_LIST_R = 107, 
         ACTUAL_PARAMETER_LIST2_R = 109, 
         IF_STATEMENT_R = 112, 
         IF_STATEMENT2_R = 117, 
         WHILE_STATEMENT_R = 119, 
         COMPOUND_STATEMENT_R = 123,
         COMPOUND_STATEMENT2_R = 127, 
         EXPRESSION_R = 130, 
         EXPRESSION2_R = 132, 
         RELATIONAL_OPERATOR_R = 134, 
         RELATIONAL_OPERATOR_R2 = 135, 
         RELATIONAL_OPERATOR_R3 = 136, 
         RELATIONAL_OPERATOR_R4 = 137,
         RELATIONAL_OPERATOR_R5 = 138, 
         RELATIONAL_OPERATOR_R6 = 139, 
         SIMPLE_EXPRESSION_R = 140, 
         SIMPLE_EXPRESSION2_R = 143, 
         SIMPLE_EXPRESSION3_R = 144, 
         SIGN_OPERATOR_R = 147, 
         SIGN_OPERATOR_R2 = 148, 
         { ADDITIVE_OPERATOR_R, ADDITIVE_OPERATOR_R2, 
           Son PLUS y MINUS y se repiten }
         ADDITIVE_OPERATOR_R3 = 149, 
         TERM_R = 150, 
         TERM2_R = 152, 
         MULTIPLYING_OPERATOR_R = 155, 
         MULTIPLYING_OPERATOR_R2 = 156, 
         MULTIPLYING_OPERATOR_R3 = 157, 
         MULTIPLYING_OPERATOR_R4 = 158, 
         FACTOR_R = 159, 
         FACTOR_R2 = 160, 
         FACTOR_R3 = 162, 
         FACTOR_R4 = 165, 
         FACTOR2_R = 167, 
         SELECTOR_R = 169, 
         SELECTOR_R2 = 170, 
         INDEX_SELECTOR_R = 171, 
         FIELD_SELECTOR_R = 174,
         CONSTANT_R = 176, 
         CONSTANT_R2 = 177);
         
         { Pila de simbolos gramaticales }
         TSymGrPtr = ^TStack;
         TStack = record
                     sym : TSymGram;
                     next : TSymGrPtr;
                  end;

   const
      { Numero de elementos del vector de reglas }
      MAXELEM = 177;
      { Vector de reglas indexado } 
      RULES : array[1..MAXELEM] of TSymGram =    
      { 1 } (PERIODX, BLOCK_BODY, SEMICOLONX, IDX, PROGRAMX,
      { 6 }  COMPOUND_STATEMENT, BLOCK_BODY5, BLOCK_BODY4, BLOCK_BODY3, BLOCK_BODY2,
     { 11 }  CONSTANT_DEFINITION_PART,
     { 12 }  EPSILON,
     { 13 }  TYPE_DEFINITION_PART,
     { 14 }  VARIABLE_DEFINITION_PART,
     { 15 }  BLOCK_BODY5, PROCEDURE_DEFINITION,
     { 17 }  CONSTANT_DEFINITION_PART2, CONSTANT_DEFINITION, CONSTX,
     { 20 }  CONSTANT_DEFINITION_PART2, CONSTANT_DEFINITION,
     { 22 }  SEMICOLONX, CONSTANT, EQUALX, IDX,
     { 26 }  TYPE_DEFINITION_PART2, TYPE_DEFINITION, TYPEX,
     { 29 }  TYPE_DEFINITION_PART2, TYPE_DEFINITION,
     { 31 }  SEMICOLONX, NEW_TYPE, EQUALX, IDX,
     { 35 }  NEW_ARRAY_TYPE,
     { 36 }  NEW_RECORD_TYPE,
     { 37 }  IDX, OFX, RIGHTBRACKETX, INDEX_RANGE, LEFTBRACKETX, ARRAYX, 
     { 43 }  CONSTANT, DOUBLEDOTX, CONSTANT,
     { 46 }  ENDX, FIELD_LIST, RECORDX,
     { 49 }  FIELD_LIST2, RECORD_SECTION,
     { 51 }  FIELD_LIST2, RECORD_SECTION, SEMICOLONX,
     { 54 }  IDX, COLONX, RECORD_SECTION2, IDX,
     { 58 }  RECORD_SECTION2, IDX, COMMAX,
     { 61 }  VARIABLE_DEFINITION_PART2, VARIABLE_DEFINITION, VARX,
     { 64 }  VARIABLE_DEFINITION_PART2, VARIABLE_DEFINITION,
     { 66 }  SEMICOLONX, VARIABLE_GROUP,
     { 68 }  IDX, COLONX, VARIABLE_GROUP2, IDX,
     { 72 }  VARIABLE_GROUP2, IDX, COMMAX, 
     { 75 }  SEMICOLONX, PROCEDURE_BLOCK, IDX, PROCEDUREX,
     { 79 }  BLOCK_BODY, SEMICOLONX, PROCEDURE_BLOCK2,
     { 82 }  RIGHTPARENTHESISX, FORMAL_PARAMETER_LIST, LEFTPARENTHESISX,
     { 85 }  FORMAL_PARAMETER_LIST2, PARAMETER_DEFINITION, 
     { 87 }  FORMAL_PARAMETER_LIST2, PARAMETER_DEFINITION, SEMICOLONX,
     { 90 }  VARIABLE_GROUP, PARAMETER_DEFINITION2,
     { 92 }  VARX, 
     { 93 }  STATEMENT2, IDX,
     { 95 }  IF_STATEMENT,
     { 96 }  WHILE_STATEMENT,
     { 97 }  COMPOUND_STATEMENT,
     { 98 }  PROCEDURE_STATEMENT,
     { 99 }  EXPRESSION, BECOMESX, EXPRESSION,
    { 102 }  STATEMENT3, SELECTOR,
    { 104 }  RIGHTPARENTHESISX, ACTUAL_PARAMETER_LIST, LEFTPARENTHESISX,
    { 107 }  ACTUAL_PARAMETER_LIST2, EXPRESSION,
    { 109 }  ACTUAL_PARAMETER_LIST2, EXPRESSION, COMMAX,
    { 112 }  IF_STATEMENT2, STATEMENT, THENX, EXPRESSION, IFX,
    { 117 }  STATEMENT, ELSEX,
    { 119 }  STATEMENT, DOX, EXPRESSION, WHILEX,
    { 123 }  ENDX, COMPOUND_STATEMENT2, STATEMENT, BEGINX,
    { 127 }  COMPOUND_STATEMENT2, STATEMENT, SEMICOLONX,
    { 130 }  EXPRESSION2, SIMPLE_EXPRESSION,
    { 132 }  SIMPLE_EXPRESSION, RELATIONAL_OPERATOR,
    { 134 }  LESSX, 
    { 135 }  EQUALX, 
    { 136 }  GREATERX,
    { 137 }  NOTGREATERX,
    { 138 }  NOTEQUALX,
    { 139 }  NOTLESSX,
    { 140 }  SIMPLE_EXPRESSION3, TERM, SIMPLE_EXPRESSION2,
    { 143 }  SIGN_OPERATOR,    
    { 144 }  SIMPLE_EXPRESSION3, TERM, ADDITIVE_OPERATOR,
    { 147 }  PLUSX,
    { 148 }  MINUSX,
    { 149 }  ORX,
    { 150 }  TERM2, FACTOR,    
    { 152 }  TERM2, FACTOR, MULTIPLYING_OPERATOR, 
    { 155 }  ASTERISKX,
    { 156 }  DIVX,
    { 157 }  MODX,
    { 158 }  ANDX,
    { 159 }  NUMERALX,
    { 160 }  FACTOR2, IDX,
    { 162 }  RIGHTPARENTHESISX, EXPRESSION, LEFTPARENTHESISX,
    { 165 }  FACTOR, NOTX,
    { 167 }  FACTOR2, SELECTOR, 
    { 169 }  INDEX_SELECTOR,
    { 170 }  FIELD_SELECTOR,
    { 171 }  RIGHTBRACKETX, EXPRESSION, LEFTBRACKETX,
    { 174 }  IDX, PERIODX,
    { 176 }  NUMERALX,
    { 177 }  IDX);
      {
      MAXRULES = 80;  Numero de reglas 
      RULESROWS : array[1..MAXRULES] of word = (1, 6, 11, 12, 13, 14, 
         15, 17, 20, 22, 26, 29, 
         31, 35, 36, 37, 43, 46,
         49, 51, 54, 58, 61, 64,
         66, 68, 72, 75, 79, 82,
         85, 87, 90, 92, 93, 95,
         96, 97, 98, 99, 102,
         104, 107, 109, 112, 117,
         119, 123, 127, 130, 132,
         134, 135, 136, 137, 138,
         139, 140, 143, 144, 147,
         148, 149, 150, 152, 155,
         156, 157, 158, 159, 160,
         162, 165, 167, 169, 170,
         171, 174, 176, 177);  
      }
   var       
      lookahead : Token; { Nos lo entrega el lexico }
      top : TSymGrPtr;   { Puntero al simbolo gramatical del top
                           de la pila }
      s_pila : TSymGram; { Simbolo que almacenara el contenido
                           del top de la pila }

implementation
  
   { Inserta un elemento en la pila de simbolos gramaticales 
     Entrada: 
                top -> puntero al tope de la pila
                sym -> simbolo gramatical a insertar
     Salida:
                top -> lo modifica adecuadamente (pila)
   }
   procedure push(var top : TSymGrPtr; sym : TSymGram);
   var 
      p : TSymGrPtr;
   begin
      new(p);
      p^.sym := sym;
      p^.next := top;
      top := p;
   end;

   { Saca un elemento de la pila de simbolos gramaticales 
     Entrada:
                top -> puntero al tope de la pila
     Salida:
                top -> se modifica adecuadamente (pila)
   }
   procedure pop(var top : TSymGrPtr);
   var
      p : TSymGrPtr;
   begin
      p := top;
      top := top^.next;
      dispose(p);
   end;

   { Dice si un simbolo gramatical es terminal, o no 
     Entrada:
                sym -> simbolo gramatical
     Salida:
        Devuelve true si el simbolo pasado como
        parametro es terminal
   }
   function isterm(sym : TSymGram) : boolean;
   begin
      if(sym <= WHILEX) then
         isterm := true
      else
         isterm := false;
   end;

   { Realiza el analisis sintactico dirigido por tabla }
   procedure anasin();
   var 
      p, q : TSymGrPtr;

   begin
      { Inicializamos la pila }
      top := NIL;  
      push(top, ENDTEXTX);
      push(top, PROGRAM1);
      
      { Comenzamos el analisis sintactico }
      repeat
      begin
         s_pila := top^.sym;  { Simbolo del top de pila }
         lookahead := yylex(atrib); { Simbolo de preanalisis }
         if(isterm(s_pila)) then { Top es un terminal o $ }
         begin
            if(s_pila = TSymGram(lookahead)) then
            begin
               pop(top);
               lookahead := yylex(atrib);
            end
            else
               syntax_error(); { CREA ESTA FUNCION, EN EL MODULO ERRORS !!! }
         end
         else                    { Top NO es un terminal }
         begin
            { Aqui falta codigo... }  
         end;
      end
      until(s_pila = ENDTEXTX);

      { Vaciamos la pila }
      p := top;
      while(p <> NIL) do
      begin
         q := p^.next;
         dispose(p);
         p := q;
      end;
   end;

begin

end.
