{ 
   Fichero: syntax.pas                            
   Analizador sintactico descendente recursivo    
   predictivo para Pascal- con recuperacion de    
   errores.                                       
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      20/11/2009                         
}

unit syntax;

interface
   uses analex, io, errors, ambito;

   type
      Token_set = set of Token;

   var
      lookahead : Token;
      Stop : Token_set;
      obj : ptr; 
      old_index : integer; { Indice temporal para evitar que 
                             perdamos el puntero al llamar a
                             algunos Match }
      old_kind : class; { Guardamos el kind cuando sabemos
                          cual es, ya que despues en 
                          Variable_group no lo sabemos 
                          porque se puede llamar de varios
                          sitios }

      procedure Match(tok : Token; Stop : Token_set);
      procedure Syntax_check(Stop : Token_set);
      procedure Syntax_error(Stop : Token_set);
      procedure Program1(Stop : Token_set);                 
      procedure Block_body(Stop : Token_set);               
      procedure Constant_definition_part(Stop : Token_set); 
      procedure Constant_definition(Stop : Token_set);      
      procedure Type_definition_part(Stop : Token_set);     
      procedure Type_definition(Stop : Token_set);          
      procedure New_type(Stop : Token_set);                 
      procedure New_array_type(Stop : Token_set);           
      procedure Index_range(Stop : Token_set);              
      procedure New_record_type(Stop : Token_set);          
      procedure Field_list(Stop : Token_set);               
      procedure Record_section(Stop : Token_set);           
      procedure Variable_definition_part(Stop : Token_set); 
      procedure Variable_definition(Stop : Token_set);      
      procedure Variable_group(Stop : Token_set);           
      procedure Procedure_definition(Stop : Token_set);     
      procedure Procedure_block(Stop : Token_set);          
      procedure Formal_parameter_list(Stop : Token_set);    
      procedure Parameter_definition(Stop : Token_set);     
      procedure Statement(Stop : Token_set);                
      procedure Statement2(Stop : Token_set);               
      { procedure Assignment_statement(Stop : Token_set); }     
      procedure Procedure_statement(Stop : Token_set);      
      procedure Actual_parameter_list(Stop : Token_set);    
      procedure If_statement(Stop : Token_set);             
      procedure While_statement(Stop : Token_set);          
      procedure Compound_statement(Stop : Token_set);       
      procedure Expression(Stop : Token_set);               
      procedure Relational_operator(Stop : Token_set);      
      procedure Simple_expression(Stop : Token_set);        
      procedure Sign_operator(Stop : Token_set);            
      procedure Additive_operator(Stop : Token_set);        
      procedure Term(Stop : Token_set);                     
      procedure Multiplying_operator(Stop : Token_set);     
      procedure Factor(Stop : Token_set);                   
      procedure Factor2(Stop : Token_set);                  
      { procedure Variable_access(Stop : Token_set); }         
      procedure Selector(Stop : Token_set);                 
      procedure Index_selector(Stop : Token_set);           
      procedure Field_selector(Stop : Token_set);           
      procedure Constant(Stop : Token_set); 

implementation

   procedure Match(tok : Token; Stop : Token_set);
   begin
      if(lookahead = tok) then
      begin
         lookahead := yylex(atrib);
         Syntax_check(Stop);
      end
      else
         Syntax_error(Stop);
   end;

   procedure Syntax_check(Stop : Token_set);
   begin
      if not(lookahead in Stop) then
         Syntax_error(Stop);
   end;

   procedure Syntax_error(Stop : Token_set);
   var
      msg : string; { Para los errores }
   begin
      if(last_line_err <> line_num) then
      begin
         if(lookahead = ENDTEXTX) then
            write_error(SYNTAX_, 'EOF encontrado antes de lo debido')
         else if(lookahead = TOKEN_ERRORX) then
            write_error(LEXICAL, 'Caracter desconocido, no se pudo asociar a ningun token')
         else
         begin
            msg := 'Token ' + TOKEN_STR[word(lookahead) + 1] + ' inesperado';
            write_error(SYNTAX_, msg);
         end;
         last_line_err := line_num;
      end;
      while not(lookahead in Stop) do
         lookahead := yylex(atrib);
   end;
      
   { <Program> ::= PROGRAMX IDX SEMICOLONX <Block_body> PERIODX }
   procedure Program1(Stop : Token_set);
   begin
      set_();
      Match(PROGRAMX, Stop + [IDX, SEMICOLONX, CONSTX, TYPEX, 
                              VARX, PROCEDUREX, BEGINX, PERIODX]);
      if(lookahead = IDX) then
         insert(atrib^.ptr^.Index, CL_PROGRAM, obj)
      else
         insert(NO_NAME, CL_PROGRAM,obj);
      Match(IDX, Stop + [SEMICOLONX, CONSTX, TYPEX, VARX, 
                         PROCEDUREX, BEGINX, PERIODX]);
      Match(SEMICOLONX, Stop + [CONSTX, TYPEX, VARX, PROCEDUREX, 
                                BEGINX, PERIODX]);
      Block_body(Stop + [PERIODX]);
      Match(PERIODX, Stop);
      reset_();
   end;
   
   { <Block_body> ::= [<Constant_definition_part>]
                      [<Type_definition_part>]
                      [<Variable_definition_part>]
                      {<Procedure_definition>}
                      <Compound_statement>
   }
   procedure Block_body(Stop : Token_set);
   begin
      if(lookahead = CONSTX) then
         Constant_definition_part(Stop + [TYPEX, VARX, PROCEDUREX,
                                          BEGINX]);
      if(lookahead = TYPEX) then
         Type_definition_part(Stop + [VARX, PROCEDUREX, BEGINX]);
      if (lookahead = VARX) then
         Variable_definition_part(Stop + [PROCEDUREX, BEGINX]);
      while (lookahead = PROCEDUREX) do
         Procedure_definition(Stop + [PROCEDUREX, BEGINX]);
      Compound_statement(Stop);
   end;

  { <Constant_definition_part> ::= CONSTX
                                    <Constant_definition>
                                    {<Constant_definition>}
   }
   procedure Constant_definition_part(Stop : Token_set);
   begin
      Match(CONSTX, Stop + [IDX]);
      Constant_definition(Stop + [IDX]);
      while(lookahead = IDX) do
         Constant_definition(Stop + [IDX]);
   end;

   { <Constant_definition> ::= IDX EQUALX <Constant> 
                               SEMICOLONX 
   }
   procedure Constant_definition(Stop : Token_set);
   begin
      { Pongo el insert antes para no perder el atrib, ya que
        el match hace un yylex, y me lo podria escachar si por
        error vienen dos ID seguidos }
      if(lookahead = IDX) then
         insert(atrib^.ptr^.Index, CL_CONSTANT, obj)
      else
         insert(NO_NAME, CL_CONSTANT, obj);
      Match(IDX, Stop + [EQUALX, NUMERALX, IDX, SEMICOLONX]);
      Match(EQUALX, Stop + [NUMERALX, IDX, SEMICOLONX]);
      Constant(Stop + [SEMICOLONX]);
      Match(SEMICOLONX, Stop);
   end;

   { <Type_definition_part> ::= TYPEX <Type_definition> 
                               {<Type_definition>}
   }
   procedure Type_definition_part(Stop : Token_set);
   begin
      Match(TYPEX, Stop + [IDX]);
      Type_definition(Stop + [IDX]);
      while(lookahead = IDX) do
         Type_definition(Stop + [IDX]);
   end;

   { <Type_definition> ::= IDX EQUALX <New_type> 
                           SEMICOLONX 
   }
   procedure Type_definition(Stop : Token_set);
   begin
      if(lookahead = IDX) then
         old_index := atrib^.ptr^.Index
      else
         old_index := NO_NAME;
      Match(IDX, Stop + [EQUALX, ARRAYX, RECORDX, SEMICOLONX]);
      Match(EQUALX, Stop + [ARRAYX, RECORDX, SEMICOLONX]);
      New_type(Stop + [SEMICOLONX]);
      Match(SEMICOLONX, Stop);
   end;

   { <New_type> ::= <New_array_type> | <New_record_type> }
   procedure New_type(Stop : Token_set);
   begin
      if(lookahead = ARRAYX) then
      begin
         insert(old_index, CL_ARRAY_TYPE, obj);
         New_array_type(Stop)
      end
      else if(lookahead = RECORDX) then
      begin
         insert(old_index, CL_RECORD_TYPE, obj);
         New_record_type(Stop)
      end
      else
         Syntax_error(Stop);
   end;

   { <New_array_type> ::= ARRAYX LEFTBRACKETX <Index_range> 
                          RIGHTBRACKETX OFX IDX 
   }
   procedure New_array_type(Stop : Token_set);
   begin
      Match(ARRAYX, Stop + [LEFTBRACKETX, NUMERALX, IDX,
                            RIGHTBRACKETX, OFX]);
      Match(LEFTBRACKETX, Stop + [IDX, NUMERALX, RIGHTBRACKETX, 
                                  OFX]);
      Index_range(Stop + [RIGHTBRACKETX, OFX, IDX]);
      Match(RIGHTBRACKETX, Stop + [OFX, IDX]);
      Match(OFX, Stop + [IDX]);
      if(lookahead = IDX) then
         lookup(atrib^.ptr^.Index, obj);
      Match(IDX, Stop);
   end;

   { <Index_range> ::= <Constant> .. <Constant> }
   procedure Index_range(Stop : Token_set);
   begin
      Constant(Stop + [DOUBLEDOTX, IDX, NUMERALX]);
      Match(DOUBLEDOTX, Stop + [IDX, NUMERALX]);
      Constant(Stop);
   end;

   { <New_record_type> ::= RECORDX <Field_list> ENDX }
   procedure New_record_type(Stop : Token_set);
   begin
      Match(RECORDX, Stop + [IDX, ENDX]);
      Field_list(Stop + [ENDX]);
      Match(ENDX, Stop);
   end;

   { <Field_list> ::= <Record_section> {; <Record_section>} }
   procedure Field_list(Stop : Token_set);
   begin
      Record_section(Stop + [SEMICOLONX]);
      while(lookahead = SEMICOLONX) do
      begin
         Match(SEMICOLONX, Stop + [IDX, SEMICOLONX]);
         Record_section(Stop + [SEMICOLONX]);
      end;
   end;

   { <Record_section> ::= IDX {, IDX} : IDX }
   procedure Record_section(Stop : Token_set);
   begin
      if(lookahead = IDX) then
         insert(atrib^.ptr^.Index, CL_FIELD, obj)
      else
         insert(NO_NAME, CL_FIELD, obj);
      Match(IDX, Stop + [COMMAX, COLONX, IDX]);
      while(lookahead = COMMAX) do
      begin
         Match(COMMAX, Stop + [COMMAX, COLONX, IDX]);
         if(lookahead = IDX) then
            insert(atrib^.ptr^.Index, CL_FIELD, obj)
         else
            insert(NO_NAME, CL_FIELD, obj);
         Match(IDX, Stop + [COMMAX, COLONX, IDX]);
      end;
      Match(COLONX, Stop + [IDX]);
      if(lookahead = IDX) then
         lookup(atrib^.ptr^.Index, obj);
      Match(IDX, Stop);
   end;

   { <Variable_definition_part> ::= VARX <Variable_definition> 
                                   {<Variable_definition>}
   }
   procedure Variable_definition_part(Stop : Token_set);
   begin
      Match(VARX, Stop + [IDX]);
      Variable_definition(Stop + [IDX]);
      while(lookahead = IDX) do
         Variable_definition(Stop + [IDX]);
   end;

   { <Variable_definition> ::= <Variable_group> SEMICOLONX }
   procedure Variable_definition(Stop : Token_set);
   begin
      old_kind := CL_VARIABLE; 
      Variable_group(Stop + [SEMICOLONX]);
      Match(SEMICOLONX, Stop);
   end;

   { <Variable_group> ::= IDX {, IDX} COLONX IDX }
   procedure Variable_group(Stop : Token_set);
   begin
      if(lookahead = IDX) then
         insert(atrib^.ptr^.Index, old_kind, obj)
      else
         insert(NO_NAME, old_kind, obj);
      Match(IDX, Stop + [COMMAX, COLONX, IDX]);
      while(lookahead = COMMAX) do
      begin
         Match(COMMAX, Stop + [COMMAX, COLONX, IDX]);
         if(lookahead = IDX) then
            insert(atrib^.ptr^.Index, old_kind, obj)
         else
            insert(NO_NAME, old_kind, obj);
         Match(IDX, Stop + [COMMAX, COLONX, IDX]);
      end;
      Match(COLONX, Stop + [IDX]);
      if(lookahead = IDX) then
         lookup(atrib^.ptr^.Index, obj);
      Match(IDX, Stop);
   end;

   { <Procedure_definition> ::= PROCEDUREX IDX 
                               <Procedure_block> ;
   }
   procedure Procedure_definition(Stop : Token_set);
   begin
      Match(PROCEDUREX, Stop + [IDX, LEFTPARENTHESISX, 
                                SEMICOLONX]);
      insert(atrib^.ptr^.Index, CL_PROCEDURE, obj);
      Match(IDX, Stop + [LEFTPARENTHESISX, SEMICOLONX]);
      Procedure_block(Stop + [SEMICOLONX]);
      Match(SEMICOLONX, Stop);
   end;

   { <Procedure_block> ::= [LEFTPARENTHESISX 
                           <Formal_parameter_list> 
                           RIGHTPARENTHESISX] SEMICOLONX 
                           <Block_body>
   }
   procedure Procedure_block(Stop : Token_set);
   begin
      set_();
      if(lookahead = LEFTPARENTHESISX) then
      begin
         Match(LEFTPARENTHESISX, Stop + [VARX, IDX, 
                                         RIGHTPARENTHESISX,
                                         SEMICOLONX, CONSTX,
                                         TYPEX, PROCEDUREX,
                                         BEGINX]);
         Formal_parameter_list(Stop + [RIGHTPARENTHESISX, 
                                       SEMICOLONX, CONSTX, TYPEX,
                                       VARX, PROCEDUREX, BEGINX]);
         Match(RIGHTPARENTHESISX, Stop + [SEMICOLONX, CONSTX, 
                                          TYPEX, VARX,
                                          PROCEDUREX, BEGINX]);
      end;
      Match(SEMICOLONX, Stop + [CONSTX, TYPEX, VARX, PROCEDUREX,
                                BEGINX]);
      Block_body(Stop);
      reset_();
   end;

   { <Formal_parameter_list> ::= <Parameter_definition> 
                                 {SEMICOLONX 
                                 <Parameter_definition>}
   }
   procedure Formal_parameter_list(Stop : Token_set);
   begin
      Parameter_definition(Stop + [SEMICOLONX]);
      while(lookahead = SEMICOLONX) do
      begin
         Match(SEMICOLONX, Stop + [VARX, IDX, SEMICOLONX]);
         Parameter_definition(Stop + [SEMICOLONX]);
      end;
   end;

   { <Parameter_definition> ::= [VARX] <Variable_group> }
   procedure Parameter_definition(Stop : Token_set);
   begin
      if(lookahead = VARX) then
      begin
         old_kind := CL_VAR_PARAMETER;
         Match(VARX, Stop + [IDX]);
      end
      else
         old_kind := CL_VALUE_PARAMETER;
      Variable_group(Stop);
   end;

   { <Statement> ::= IDX <Statement2> | <If_statement> | 
                     <While_statement> | <Compound_statement> |
                     Epsilon
   }
   procedure Statement(Stop : Token_set);
   begin
      if(lookahead = IDX) then
      begin
         lookup(atrib^.ptr^.Index, obj);
         Match(IDX, Stop + [LEFTPARENTHESISX, LEFTBRACKETX, 
                            PERIODX, BECOMESX]);
         Statement2(Stop);
      end
      else if(lookahead = IFX) then
         If_statement(Stop)
      else if(lookahead = WHILEX) then
         While_statement(Stop)
      else if(lookahead = BEGINX) then
         Compound_statement(Stop)
      else
         Syntax_check(Stop);
   end;

   { <Statement2> ::= <Procedure_statement> | {<Selector>}
                      BECOMESX <Expression> | Epsilon
   } 
   procedure Statement2(Stop : Token_set);
   begin
      if(lookahead = LEFTPARENTHESISX) then
         Procedure_statement(Stop)
      else if(lookahead in [LEFTBRACKETX, PERIODX, BECOMESX]) then
      begin
         while(lookahead in [LEFTBRACKETX, PERIODX]) do
            Selector(Stop + [BECOMESX, LEFTBRACKETX, PERIODX,
                             PLUSX, MINUSX, NUMERALX, IDX, 
                             LEFTPARENTHESISX, NOTX]);
         Match(BECOMESX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                                 LEFTPARENTHESISX, NOTX]);
         Expression(Stop);
      end
      else
         Syntax_check(Stop);
   end;
   
   { <Assignment_statement> ::= <Variable_access> BECOMESX
                                <Expression> 
   
   procedure Assignment_statement(Stop : Token_set);
   begin
      Variable_access(Stop + [BECOMESX, PLUSX, MINUSX, NUMERALX,
                              IDX, LEFTPARENTHESISX, NOTX]);
      Match(BECOMESX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                              LEFTPARENTHESISX, NOTX]);
      Expression(Stop);
   end;
   }

   { <Procedure_statement> ::= [LEFTPARENTHESISX 
                               <Actual_parameter_list>
                               RIGHTPARENTHESISX] }
   procedure Procedure_statement(Stop : Token_set);
   begin
      if(lookahead = LEFTPARENTHESISX) then 
      begin
         Match(LEFTPARENTHESISX, Stop + [PLUSX, MINUSX, NUMERALX,
                                         IDX, LEFTPARENTHESISX,
                                         NOTX, 
                                         RIGHTPARENTHESISX]);
         Actual_parameter_list(Stop + [RIGHTPARENTHESISX]);
         Match(RIGHTPARENTHESISX, Stop);
      end;      
   end;

   { <Actual_parameter_list> ::= <Expression>
                                 {COMMAX <Expression>}
   }
   procedure Actual_parameter_list(Stop : Token_set);
   begin
      Expression(Stop + [COMMAX]);
      while(lookahead = COMMAX) do 
      begin
         Match(COMMAX, Stop + [PLUSX, MINUSX, NUMERALX, IDX, 
                               LEFTPARENTHESISX, NOTX, COMMAX]);
         { Ponemos la COMMAX de nuevo aqui --------------^ 
           por la repeticion cero o mas veces }
         Expression(Stop + [COMMAX]);
      end;        
   end;

   { <If_statement> ::= IFX <Expression> THENX <Statement>
                        [ELSE <Statement>]
   }
   procedure If_statement(Stop : Token_set);
   begin
      Match(IFX, Stop + [PLUSX, MINUSX, NUMERALX, IDX, 
                         LEFTPARENTHESISX, NOTX, THENX, IFX,
                         WHILEX, BEGINX, ELSEX]);
      Expression(Stop + [THENX, IDX, IFX, WHILEX, BEGINX, ELSEX]);
      Match(THENX, Stop + [IDX, IFX, WHILEX, BEGINX, ELSEX]);
      Statement(Stop + [ELSEX, IDX, IFX, WHILEX, BEGINX]);
      Syntax_check(Stop + [ELSEX]);
      if(lookahead = ELSEX) then 
      begin
         Match(ELSEX, Stop + [IDX, IFX, WHILEX, BEGINX]);
         Statement(Stop);
      end;
   end;

   { <While_statement> ::= WHILEX <Expression> DOX <Statement> }
   procedure While_statement(Stop : Token_set);
   begin
      Match(WHILEX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                            LEFTPARENTHESISX, NOTX, DOX, IFX,
                            WHILEX, BEGINX]);
      Expression(Stop + [DOX, IDX, IFX, WHILEX, BEGINX]);
      Match(DOX, Stop + [IDX, IFX, WHILEX, BEGINX]);
      Statement(Stop);
   end;
   
   { <Compound_statement> ::= BEGINX <Statement> 
                              {SEMICOLONX <Statement>} ENDX 
   }
   procedure Compound_statement(Stop : Token_set);
   begin
      Match(BEGINX, Stop + [IDX, IFX, WHILEX, BEGINX, SEMICOLONX,
                            ENDX]);
      Statement(Stop + [SEMICOLONX, ENDX]);
      while(lookahead = SEMICOLONX) do 
      begin
         Match(SEMICOLONX, Stop + [IDX, IFX, WHILEX, BEGINX,
                                   SEMICOLONX, ENDX]);
         Statement(Stop + [SEMICOLONX, ENDX]);
      end;
      Match(ENDX, Stop);
   end;

   { <Expression> ::= <Simple_expression> 
                       [<Relational_operator> <Simple_expression>] 
   }
   procedure Expression(Stop : Token_set);
   begin
      Simple_expression(Stop + [LESSX, EQUALX, GREATERX, 
                                NOTGREATERX, NOTEQUALX, 
                                NOTLESSX]);
      if(lookahead in [LESSX, EQUALX, GREATERX, NOTGREATERX, 
                       NOTEQUALX, NOTLESSX]) then 
      begin
         Relational_operator(Stop + [PLUSX, MINUSX, NUMERALX,
                                     IDX, LEFTPARENTHESISX, 
                                     NOTX]);
         Simple_expression(Stop);
      end;
   end;

   { <Relational_operator> ::= LESSX | EQUALX | GREATERX | 
                               NOTGREATERX | NOTEQUALX | 
                               NOTLESSX }
   procedure Relational_operator(Stop : Token_set);
   begin
      { Una manera mas inteligente, poner Match(lookahead, Stop) 
        Pero como me gusta mas asi porque lo veo mas claro... }
      if(lookahead = LESSX) then
         Match(LESSX, Stop)
      else if(lookahead = EQUALX) then
         Match(EQUALX, Stop)
      else if(lookahead = GREATERX) then
         Match(GREATERX, Stop)
      else if(lookahead = NOTGREATERX) then
         Match(NOTGREATERX, Stop)
      else if(lookahead = NOTEQUALX) then
         Match(NOTEQUALX, Stop)
      else if(lookahead = NOTLESSX) then
         Match(NOTLESSX, Stop)
      else
         Syntax_error(Stop);
   end;

   { <Simple_expression> ::= [<Sign_operator>] <Term> 
                             {<Additive_operator> <Term>} 
   }
   procedure Simple_expression(Stop : Token_set);
   begin
      if(lookahead in [PLUSX, MINUSX]) then
         Sign_operator(Stop + [NUMERALX, IDX, LEFTPARENTHESISX,
                               NOTX, PLUSX, MINUSX, ORX]);
      Term(Stop + [PLUSX, MINUSX, ORX]);
      while(lookahead in [PLUSX, MINUSX, ORX]) do 
      begin
         Additive_operator(Stop + [NUMERALX, IDX, 
                                   LEFTPARENTHESISX, NOTX, PLUSX,
                                   MINUSX, ORX]);
         Term(Stop + [PLUSX, MINUSX, ORX]);
      end;
   end;

   { <Sign_operator> ::= PLUSX | MINUSX } 
   procedure Sign_operator(Stop : Token_set);
   begin
      if(lookahead = PLUSX) then
         Match(PLUSX, Stop)
      else if(lookahead = MINUSX) then
         Match(MINUSX, Stop)
      else
         Syntax_error(Stop);
   end;

   { <Additive_operator> ::= PLUSX | MINUSX | ORX }
   procedure Additive_operator(Stop : Token_set);
   begin
      if(lookahead = PLUSX) then
         Match(PLUSX, Stop)
      else if(lookahead = MINUSX) then
         Match(MINUSX, Stop)
      else if(lookahead = ORX) then
         Match(ORX, Stop)
      else
         Syntax_error(Stop);
   end;

   { <Term> ::= <Factor> {<Multiplying_operator> <Factor>} }
   procedure Term(Stop : Token_set);
   begin
      Factor(Stop + [ASTERISKX, DIVX, MODX, ANDX]);
      while(lookahead in [ASTERISKX, DIVX, MODX, ANDX]) do 
      begin
         Multiplying_operator(Stop + [NUMERALX, IDX,
                                      LEFTPARENTHESISX, NOTX,
                                      ASTERISKX, DIVX, MODX, 
                                      ANDX]);
         Factor(Stop + [ASTERISKX, DIVX, MODX, ANDX]);
      end;
   end;

   { <Multiplying_operator> ::= ASTERISKX | DIVX | MODX | ANDX }
   procedure Multiplying_operator(Stop : Token_set);
   begin
      { Vuelve a ser mas inteligente Match(lookahead, Stop)
        Pero por no complicar la cosa... }
      if(lookahead = ASTERISKX) then
         Match(ASTERISKX, Stop)
      else if(lookahead = DIVX) then
         Match(DIVX, Stop)
      else if (lookahead = MODX) then
         Match(MODX, Stop)
      else if(lookahead = ANDX) then
         Match(ANDX, Stop)
      else
         Syntax_error(Stop);
   end;

   { <Factor> ::= NUMERALX | IDX <Factor2> | LEFTPARENTHESISX 
                  <Expression> RIGHTPARENTHESISX | NOTX 
                  <Factor> }
   procedure Factor(Stop : Token_set);
   begin
      if(lookahead = NUMERALX) then
         Match(NUMERALX, Stop)
      else if(lookahead = IDX) then 
      begin
         lookup(atrib^.ptr^.Index, obj);
         Match(IDX, Stop + [LEFTBRACKETX, PERIODX]);
         Factor2(Stop);
      end
      else if(lookahead = LEFTPARENTHESISX) then 
      begin
         Match(LEFTPARENTHESISX, Stop + [PLUSX, MINUSX, NUMERALX,
                                         IDX, LEFTPARENTHESISX,
                                         NOTX, 
                                         RIGHTPARENTHESISX]);
         Expression(Stop + [RIGHTPARENTHESISX]);
         Match(RIGHTPARENTHESISX, Stop);
      end
      else if(lookahead = NOTX) then 
      begin
         Match(NOTX, Stop + [NUMERALX, IDX, LEFTPARENTHESISX, 
                             NOTX]);
         Factor(Stop);
      end
      else
         Syntax_error(Stop);
   end;

   { <Factor2> ::= {<Selector>} | Epilon }
   procedure Factor2(Stop : Token_set);
   begin
      while(lookahead in [LEFTBRACKETX, PERIODX]) do
         Selector(Stop + [LEFTBRACKETX, PERIODX]);
   end;

   { <Variable_access> ::= IDX {<selector>} }
   {
   procedure Variable_access(Stop : Token_set);
   begin
      Match(IDX, Stop + [LEFTBRACKETX, PERIODX]);
      while(lookahead in [LEFTBRACKETX, PERIODX]) do
         Selector(Stop + [LEFTBRACKETX, PERIODX]);
   end;
   }

   { <Selector> ::= <Index_selector> | <Field_selector> }
   procedure Selector(Stop : Token_set);
   begin
      if(lookahead = LEFTBRACKETX) then
         Index_selector(Stop)
      else if(lookahead = PERIODX) then
         Field_selector(Stop)
      else
         Syntax_error(Stop);
   end;

   { <Index_selector> ::= LEFTBRACKETX <Expression> RIGHTBRACKETX }
   procedure Index_selector(Stop : Token_set);
   begin
      Match(LEFTBRACKETX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                                  LEFTPARENTHESISX, NOTX,
                                  RIGHTBRACKETX]);
      Expression(Stop + [RIGHTBRACKETX]);
      Match(RIGHTBRACKETX, Stop);
   end;

   { <Field_selector> ::= PERIODX IDX }
   procedure Field_selector(Stop : Token_set);
   begin
      Match(PERIODX, Stop + [IDX]);
      if(lookahead = IDX) then
         lookup(atrib^.ptr^.Index, obj);
      Match(IDX, Stop);
   end;

   { <Constant> ::= NUMERALX | IDX }
   procedure Constant(Stop : Token_set);
   begin
      if(lookahead = NUMERALX) then
         Match(NUMERALX, Stop)
      else if(lookahead = IDX) then
      begin
         lookup(atrib^.ptr^.Index, obj);
         Match(IDX, Stop);
      end
      else
         Syntax_error(Stop);
   end;

begin

end.
