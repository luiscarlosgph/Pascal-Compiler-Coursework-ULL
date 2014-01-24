unit syntax;

interface

   uses analex, io;

   var
      lookahead : Token;

   { Funciones }
      procedure Match(tok : Token);
      procedure Syntax_error();
      procedure Program_();                 
      procedure Block_body();               
      procedure Constant_definition_part(); 
      procedure Constant_definition();      
      procedure Type_definition_part();     
      procedure Type_definition();          
      procedure New_type();                 
      procedure New_array_type();           
      procedure Index_range();              
      procedure New_record_type();          
      procedure Field_list();               
      procedure Record_section();           
      procedure Variable_definition_part(); 
      procedure Variable_definition();      
      procedure Variable_group();           
      procedure Procedure_definition();     
      procedure Procedure_block();          
      procedure Formal_parameter_list();    
      procedure Parameter_definition();     
      procedure Statement();                
      procedure Statement2();               
      procedure Assignment_statement();     
      procedure Procedure_statement();      
      procedure Actual_parameter_list();    
      procedure If_statement();             
      procedure While_statement();          
      procedure Compound_statement();       
      procedure Expression();               
      procedure Relational_operator();      
      procedure Simple_expression();        
      procedure Sign_operator();            
      procedure Additive_operator();        
      procedure Term();                     
      procedure Multiplying_operator();     
      procedure Factor();                   
      procedure Factor2();                  
      procedure Variable_access();          
      procedure Selector();                 
      procedure Index_selector();           
      procedure Field_selector();           
      procedure Constant(); 

implementation

   procedure Match(tok : Token);
   begin
      if(lookahead = tok) then
         lookahead := yylex(atrib)
      else
         Syntax_error();
   end;

   procedure Syntax_error();
   begin
      writeln('   (', line_num, ', ', col_num, ')', ' - Error en token -> ', TOKEN_STR[word(lookahead)]);
      halt(); 
   end;
   
      
   { <Program> ::= PROGRAMX IDX SEMICOLONX <Block_body> }
   procedure Program_();
   begin
      Match(PROGRAMX);
      Match(IDX);
      Match(SEMICOLONX);
      Block_body();
      Match(PERIODX);
      Match(ENDTEXTX);
   end;
   
   { <Block_body> ::= [<Constant_definition_part>]
                      [<Type_definition_part>]
                      [<Variable_definition_part>]
                      {<Procedure_definition>}
                      <Compound_statement>
   }
   procedure Block_body();
   begin
      if(lookahead = CONSTX) then
         Constant_definition_part();
      if(lookahead = TYPEX) then
         Type_definition_part();
      if (lookahead = VARX) then
         Variable_definition_part();
      while (lookahead = PROCEDUREX) do
         Procedure_definition();
      compound_statement ();
   end;

	{ <Constant_definition_part> ::= CONSTX
                                    <Constant_definition>
                                    {<Constant_definition>}
   }
   procedure Constant_definition_part();
   begin
      Match(CONSTX);
      Constant_definition();
      while(lookahead = IDX) do
         Constant_definition();
   end;

   { <Constant_definition> ::= IDX EQUALX <Constant> 
                               SEMICOLONX 
   }
   procedure Constant_definition();
   begin
      Match(IDX);
      Match(EQUALX);
      Constant();
      Match(SEMICOLONX);
   end;

   { <Type_definition_part> ::= TYPEX <Type_definition> 
                               {<Type_definition>}
   }
   procedure Type_definition_part();
   begin
      Match(TYPEX);
      Type_definition();
      while(lookahead = IDX) do
         Type_definition();
   end;

   { <Type_definition> ::= IDX EQUALX <New_type> 
                           SEMICOLONX 
   }
   procedure Type_definition();
   begin
      Match(IDX);
      Match(EQUALX);
      New_type();
      Match(SEMICOLONX);
   end;

   { <New_type> ::= <New_array_type> | <New_record_type> }
   procedure New_type();
   begin
      if(lookahead = ARRAYX) then
         New_array_type()
      else if(lookahead = RECORDX) then
         New_record_type()
      else
         Syntax_error();
   end;

   { <New_array_type> ::= ARRAYX LEFTBRACKETX <Index_range> 
                          RIGHTBRACKETX OFX IDX 
   }
   procedure New_array_type();
   begin
      Match(ARRAYX);
      Match(LEFTBRACKETX);
      Index_range();
      Match(RIGHTBRACKETX);
      Match(OFX);
      Match(IDX);
   end;

   { <Index_range> ::= <Constant> .. <Constant> }
   procedure Index_range();
   begin
      Constant();
      Match(DOUBLEDOTX);
      Constant();
   end;

   { <New_record_type> ::= RECORDX <Field_list> ENDX }
   procedure New_record_type();
   begin
      Match(RECORDX);
      Field_list();
      Match(ENDX);
   end;

   { <Field_list> ::= <Record_section> {; <Record_section>} }
   procedure Field_list();
   begin
      Record_section();
      while(lookahead = SEMICOLONX) do
      begin
         Match(SEMICOLONX);
         Record_section();
      end;
   end;

   { <Record_section> ::= IDX {, IDX} : IDX }
   procedure Record_section();
   begin
      Match(IDX);
      while(lookahead = COMMAX) do
      begin
         Match(COMMAX);
         Match(IDX);
      end;
      Match(COLONX);
      Match(IDX);
   end;

   { <Variable_definition_part> ::= VARX <Variable_definition> 
                                   {<Variable_definition>}
   }
   procedure Variable_definition_part();
   begin
      Match(VARX);
      Variable_definition();
      while(lookahead = IDX) do
         Variable_definition();
   end;

   { <Variable_definition> ::= <Variable_group> ; }
   procedure Variable_definition();
   begin
      Variable_group();
      Match(SEMICOLONX);
   end;

   { <Variable_group> ::= IDX {, IDX} : IDX }
   procedure Variable_group();
   begin
      Match(IDX);
      while(lookahead = COMMAX) do
      begin
         Match(COMMAX);
         Match(IDX);
      end;
      Match(COLONX);
      Match(IDX);
   end;

   { <Procedure_definition> ::= PROCEDUREX IDX 
                               <Procedure_block> ;
   }
   procedure Procedure_definition();
   begin
      Match(PROCEDUREX);
      Match(IDX);
      Procedure_block();
      Match(SEMICOLONX);
   end;

   { <Procedure_block> ::= [LEFTPARENTHESISX <Formal_parameter_list> 
                            RIGHTPARENTHESISX] SEMICOLONX <Block_body>
   }
   procedure Procedure_block();
   begin
      if(lookahead = LEFTPARENTHESISX) then
      begin
         Match(LEFTPARENTHESISX);
         Formal_parameter_list();
         Match(RIGHTPARENTHESISX);
      end;
      Match(SEMICOLONX);
      Block_body();
   end;

   { <Formal_parameter_list> ::= <Parameter_definition> 
                                 {SEMICOLONX <Parameter_definition>}
   }
   procedure Formal_parameter_list();
   begin
      Parameter_definition();
      while(lookahead = SEMICOLONX) do
      begin
         Match(SEMICOLONX);
         Parameter_definition();
      end;
   end;

   { <Parameter_definition> ::= [VARX] <Variable_group> }
   procedure Parameter_definition;
   begin
      if(lookahead = VARX) then
         Match(VARX);
      Variable_group();
   end;

   { <Statement> ::= IDX <Statement2> | <If_statement> | 
                     <While_statement> | <Compound_statement> |
                     Epsilon
   }
   procedure Statement();
   begin
      if(lookahead = IDX) then
      begin
         Match(IDX);
         Statement2();
      end
      else if(lookahead = IFX) then
         If_statement()
      else if(lookahead = WHILEX) then
         While_statement()
      else if(lookahead = BEGINX) then
         Compound_statement();
   end;

   { <Statement2> ::= <Procedure_statement> | {<Selector>}
                      BECOMESX <Expression> | Epsilon
   } 
   procedure Statement2();
   begin
      if(lookahead = LEFTPARENTHESISX) then
         Procedure_statement()
      else if(lookahead in [LEFTBRACKETX, PERIODX, BECOMESX]) then
      begin
         while(lookahead in [LEFTBRACKETX, PERIODX]) do
            Selector();
         Match(BECOMESX);
         Expression();
      end
   end;
   
   { <Assignment_statement> ::= <Variable_access> BECOMESX
                                <Expression> 
   }
   procedure Assignment_statement();
   begin
      Variable_access();
      Match(BECOMESX);
      Expression();
   end;

   { <Procedure_statement> ::= [LEFTPARENTHESISX 
                               <Actual_parameter_list>
                               RIGHTPARENTHESISX] }
   procedure Procedure_statement();
   begin
      if(lookahead = LEFTPARENTHESISX) then 
      begin
         Match(LEFTPARENTHESISX);
         Actual_parameter_list();
         Match(RIGHTPARENTHESISX);
      end;      
   end;

   { <Actual_parameter_list> ::= <Actual_parameter> 
                                 {COMMAX <Actual_parameter>}
   }
   procedure Actual_parameter_list();
   begin
      Expression();
      while(lookahead = COMMAX) do 
      begin
         Match(COMMAX);
         Expression();
      end;        
   end;

   { <If_statement> ::= IFX <Expression> THENX <Statement>
                        [ELSE <Statement>]
   }
   procedure If_statement();
   begin
      Match(IFX);
      Expression();
      Match(THENX);
      Statement();
      if(lookahead = ELSEX) then 
      begin
         Match(ELSEX);
         Statement();
      end;
   end;

   { <While_statement> ::= WHILEX <Expression> DOX <Statement> }
   procedure While_statement();
   begin
      Match(WHILEX);
      Expression();
      Match(DOX);
      Statement();
   end;
   
   { <Compound_statement> ::= BEGINX <Statement> 
                              {SEMICOLONX <Statement>} ENDX 
   }
   procedure Compound_statement();
   begin
      Match(BEGINX);
      Statement();
      while(lookahead = SEMICOLONX) do 
      begin
         Match(SEMICOLONX);
         Statement();
      end;
      Match(ENDX);
   end;

   { <Expression> ::= <Simple_expression> 
                       [<Relational_operator> <Simple_expression>] 
   }
   procedure Expression();
   begin
      Simple_expression();
      if(lookahead in [LESSX, EQUALX, GREATERX, NOTGREATERX, 
                       NOTEQUALX, NOTLESSX]) then 
      begin
         Relational_operator();
         Simple_expression();
      end;
   end;

   { <Relational_operator> ::= LESSX | EQUALX | GREATERX | 
                               NOTGREATERX | NOTEQUALX | NOTLESSX }
   procedure Relational_operator();
   begin
      if(lookahead = LESSX) then
         Match(LESSX)
      else if(lookahead = EQUALX) then
         Match(EQUALX)
      else if(lookahead = GREATERX) then
         Match(GREATERX)
      else if(lookahead = NOTGREATERX) then
         Match(NOTGREATERX)
      else if(lookahead = NOTEQUALX) then
         Match(NOTEQUALX)
      else if(lookahead = NOTLESSX) then
         Match(NOTLESSX)
      else
         Syntax_error();
   end;

   { <Simple_expression> ::= [<Sign_operator>] <Term> 
                             {<Additive_operator> <Term>} 
   }
   procedure Simple_expression();
   begin
      if(lookahead in [PLUSX, MINUSX]) then
         Sign_operator();
      Term();
      while(lookahead in [PLUSX, MINUSX, ORX]) do 
      begin
         Additive_operator();
         Term();
      end;
   end;

   { <sign_operator> ::= PLUSX | MINUSX } 
   procedure Sign_operator();
   begin
      if(lookahead = PLUSX) then
         Match(PLUSX)
      else if(lookahead = MINUSX) then
         Match(MINUSX)
      else
         Syntax_error();
   end;

   { <Additive_operator> ::= PLUSX | MINUSX | ORX }
   procedure Additive_operator();
   begin
      if(lookahead = PLUSX) then
         Match(PLUSX)
      else if(lookahead = MINUSX) then
         Match(MINUSX)
      else if(lookahead = ORX) then
         Match(ORX)
      else
         Syntax_error();
   end;

   { <Term> ::= <Factor> {<Multiplying_operator> <Factor>} }
   procedure Term();
   begin
      Factor();
      while(lookahead in [ASTERISKX, DIVX, MODX, ANDX]) do 
      begin
         Multiplying_operator();
         Factor();
      end;
   end;

   { <Multiplying_operator> ::= ASTERISKX | DIVX | MODX | ANDX }
   procedure Multiplying_operator();
   begin
      if(lookahead = ASTERISKX) then
         Match(ASTERISKX)
      else if(lookahead = DIVX) then
         Match(DIVX)
      else if (lookahead = MODX) then
         Match(MODX)
      else if(lookahead = ANDX) then
         Match(ANDX)
      else
         Syntax_error();
   end;

   { <Factor> ::= NUMERALX | IDX <Factor2> | LEFTPARENTHESISX 
                  <Expression> RIGHTPARENTHESISX | NOTX 
                  <Factor> }
   procedure Factor();
   begin
      if(lookahead = NUMERALX) then
         Match(NUMERALX)
      else if(lookahead = IDX) then 
      begin
         Match(IDX);
         Factor2();
      end
      else if(lookahead = LEFTPARENTHESISX) then 
      begin
         Match(LEFTPARENTHESISX);
         Expression();
         Match(RIGHTPARENTHESISX);
      end
      else if(lookahead = NOTX) then 
      begin
         Match(NOTX);
         Factor();
      end
      else
         Syntax_error();
   end;

   { <Factor2> ::= {<Selector>} | Epilon }
   procedure Factor2();
   begin
      while(lookahead in [LEFTBRACKETX, PERIODX]) do
         Selector();
   end;

   { <Variable_access> ::= IDX {<selector>} }
   procedure Variable_access();
   begin
      Match(IDX);
      while(lookahead in [LEFTBRACKETX, PERIODX]) do
         Selector();
   end;

   { <Selector> ::= <Index_selector> | <Field_selector> }
   procedure Selector();
   begin
      if(lookahead = LEFTBRACKETX) then
         Index_selector()
      else if(lookahead = PERIODX) then
         Field_selector()
      else
         Syntax_error();
   end;

   { <Index_selector> ::= LEFTBRACKETX <Expression> RIGHTBRACKETX }
   procedure Index_selector();
   begin
      Match(LEFTBRACKETX);
      Expression();
      Match(RIGHTBRACKETX);
   end;

   { <Field_selector> ::= PERIODX IDX }
   procedure Field_selector();
   begin
      Match(PERIODX);
      Match(IDX);
   end;

   { <Constant> ::= NUMERALX | IDX }
   procedure Constant();
   begin
      if(lookahead = NUMERALX) then
         Match(NUMERALX)
      else if(lookahead = IDX) then
         Match(IDX)
      else
         Syntax_error();
   end;

begin

end.
