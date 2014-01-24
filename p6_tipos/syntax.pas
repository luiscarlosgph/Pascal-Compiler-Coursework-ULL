{ 
   Fichero: syntax.pas                            
   Analizador sintactico descendente recursivo    
   predictivo para Pascal- con recuperacion de    
   errores.                                       
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      04/12/2009                         
}

unit syntax;

interface
   uses analex, io, errors, symtable, ambito, types;

   type
      Token_set = set of Token;

   var
      lookahead : Token;
      Stop : Token_set;
      obj : ptr; 
      old_index : integer; { Indice temporal para evitar que 
                             perdamos el puntero al llamar a
                             algunos Match }
      { old_kind : class; } { Guardamos el kind cuando sabemos
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
      procedure New_type(index : integer; Stop : Token_set);
      procedure New_array_type(index : integer; Stop : Token_set);
      procedure Index_range(var val1 : integer; var type1 : ptr; 
                            var val2 : integer; var type2 : ptr;
                            Stop : Token_set);
      procedure New_record_type(index : integer; Stop : Token_set);
      procedure Field_list(var LastField : ptr; Stop : Token_set);
      procedure Record_section(var LastField : ptr; 
                               Stop : Token_set);
      procedure Variable_definition_part(Stop : Token_set); 
      procedure Variable_definition(Stop : Token_set);      
      procedure Variable_group(kind : class; var LastVar : ptr; 
                               Stop : Token_set);
      procedure Procedure_definition(Stop : Token_set);     
      procedure Procedure_block(obj_bak : ptr; Stop : Token_set);
      procedure Formal_parameter_list(var LastParam : ptr; 
                                      Stop : Token_set);
      procedure Parameter_definition(var LastParam : ptr; 
                                     Stop : Token_set);
      procedure IO_Statement(obj : ptr; Stop : Token_set);
      procedure Statement(Stop : Token_set);                
      procedure Statement2(obj : ptr; Stop : Token_set);
      { procedure Assignment_statement(Stop : Token_set); }     
      procedure Procedure_statement(obj : ptr; Stop : Token_set);
      procedure Actual_parameter_list(param : ptr; 
                                      Stop : Token_set);
      procedure If_statement(Stop : Token_set);             
      procedure While_statement(Stop : Token_set);          
      procedure Compound_statement(Stop : Token_set);       
      procedure Expression(var type_ : ptr; Stop : Token_set);
      procedure Relational_operator(Stop : Token_set);      
      procedure Simple_expression(var type_ : ptr; 
                                  Stop : Token_set);
      procedure Sign_operator(Stop : Token_set);            
      procedure Additive_operator(var type_ : ptr; 
                                  Stop : Token_set);

      procedure Term(var type_ : ptr; Stop : Token_set);
      procedure Multiplying_operator(var type_ : ptr; 
                                     Stop : Token_set);
      procedure Factor(var type_ : ptr; Stop : Token_set);
      procedure Factor2(var type_ : ptr; Stop : Token_set);
      { procedure Variable_access(Stop : Token_set); }         
      procedure Selector(var type_ : ptr; Stop : Token_set);
      procedure Index_selector(var type_ : ptr; 
                               Stop : Token_set);
      procedure Field_selector(var type_ : ptr; 
                               Stop : Token_set);
      procedure Constant(var value : integer; var type_ : ptr; 
                         Stop : Token_set);

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
      set_(); { No permito ids iguales al nombre del programa } 
      Match(PROGRAMX, Stop + [IDX, SEMICOLONX, CONSTX, TYPEX, 
                              VARX, PROCEDUREX, BEGINX, PERIODX]);
      if(lookahead = IDX) then
         insert(atrib^.ptr^.Index, CL_PROGRAM, obj)
      else
         insert(NO_NAME, CL_PROGRAM,obj);
      { set_(); Si permito ids iguales al nombre del programa }
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
   var 
      _index_ : integer; { Almacenan temporalmente los datos }
      _lookahead_ : Token;  { de la constante para posponer la  }
      { insercion hasta el momento en el que sepamos que      }
      { casos como A = A no se van a producir                 }
      value : integer;
      type_ : ptr;
   begin
      _lookahead_ := lookahead;
      _index_ := atrib^.ptr^.Index;
      Match(IDX, Stop + [EQUALX, NUMERALX, IDX, SEMICOLONX]);
      Match(EQUALX, Stop + [NUMERALX, IDX, SEMICOLONX]);
      Constant(value, type_, Stop + [SEMICOLONX]);
      { Si pasa que A = A, obj^.CS.ConsType = TypeError }
      { if((_lookahead_ = IDX) and 
         (type_ <> TypeError)) then }
      if(_lookahead_ = IDX) then
         insert(_index_, CL_CONSTANT, obj)
      else
         insert(NO_NAME, CL_CONSTANT, obj);
      obj^.CS.ConstValue := value;
      obj^.CS.ConstType := type_;
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
   var
      index : integer;
   begin
      if(lookahead = IDX) then
         index := atrib^.ptr^.Index
      else
         index := NO_NAME;
      Match(IDX, Stop + [EQUALX, ARRAYX, RECORDX, SEMICOLONX]);
      Match(EQUALX, Stop + [ARRAYX, RECORDX, SEMICOLONX]);
      { Pasamos el id del nuevo tipo ya que no sabemos que sera
        entonces no podemos insertar todavia }
      New_type(index, Stop + [SEMICOLONX]);
      Match(SEMICOLONX, Stop);
   end;

   { <New_type> ::= <New_array_type> | <New_record_type> }
   procedure New_type(index : integer; Stop : Token_set);
   begin
      if(lookahead = ARRAYX) then
      begin
         { insert(index, CL_ARRAY_TYPE, obj); }
         { Todavia no tenemos los datos necesarios para
           el nuevo tipo array entonces posponemos una
           vez mas la insercion y pasamos el id del nuevo
           tipo }
         New_array_type(index, Stop)
      end
      else if(lookahead = RECORDX) then
      begin
         { insert(index, CL_RECORD_TYPE, obj); }
         { No insertamos todavia para evitar que nos declaren
           algun campo del record con el tipo de dato del mismo
           record que se esta declarando }
         New_record_type(index, Stop)
      end
      else
         Syntax_error(Stop);
   end;

   { <New_array_type> ::= ARRAYX LEFTBRACKETX <Index_range> 
                          RIGHTBRACKETX OFX IDX 
   }
   procedure New_array_type(index : integer; Stop : Token_set);
   var
      val1, val2 : integer; { Cota inferior y superior del array }
      type1, type2 : ptr; { Tipo de ambas cotas }
      msg : string; { Mensaje de error de rango }
      etype : ptr; { Tipo de los elementos del array }
   begin
      Match(ARRAYX, Stop + [LEFTBRACKETX, NUMERALX, IDX,
                            RIGHTBRACKETX, OFX]);
      Match(LEFTBRACKETX, Stop + [IDX, NUMERALX, RIGHTBRACKETX, 
                                  OFX]);
      { Index_range nos da las cotas del array, luego habra
        que comprobar que sean del mismo tipo y que la cota
        inferior no sea mayor que la superior }
      Index_range(val1, type1, val2, type2, 
                  Stop + [RIGHTBRACKETX, OFX, IDX]);
      Match(RIGHTBRACKETX, Stop + [OFX, IDX]);
      Match(OFX, Stop + [IDX]);
      if(lookahead = IDX) then
      begin
         lookup(atrib^.ptr^.Index, obj);
         etype := obj;
      end
      else
         etype := TypeError; 
      { Si el tipo de los elementos del array no es ni otro
        tipo array o record ni tampoco un tipo estandar 
        estamos ante un error de tipos }
      if((etype^.kind <> CL_ARRAY_TYPE) and 
         (etype^.kind <> CL_RECORD_TYPE) and 
         (etype^.kind <> CL_STANDARD_TYPE)) then
         TypexError(etype);
      { Insertamos aqui por si se da el caso de que el
        usuario pone type T = array[1..2] of T
        al leer el segundo T el lookup que tenemos encima
        daria un error porque todavia no hemos insertado
        T }
      insert(index, CL_ARRAY_TYPE, obj); 
      { Comprobamos que las cotas son del mismo tipo }
      CheckTypes(type1, type2);  
      { Comprobamos que la cota inferior es realmente
        mas pequenya que la superior }
      if(val1 > val2) then
      begin
         val2 := val1;
         msg := 'Rango no valido';
         write_error(SEMANTIC, msg);
      end;
      obj^.AS.LowerBound := val1; 
      obj^.AS.UpperBound := val2;
      obj^.AS.IndexType := type1;
      obj^.AS.ElementType := etype; 
      Match(IDX, Stop);
   end;

   { <Index_range> ::= <Constant> .. <Constant> }
   procedure Index_range(var val1 : integer; var type1 : ptr; 
                         var val2 : integer; var type2 : ptr;
                         Stop : Token_set);
   begin
      Constant(val1, type1, Stop + [DOUBLEDOTX, IDX, NUMERALX]);
      Match(DOUBLEDOTX, Stop + [IDX, NUMERALX]);
      Constant(val2, type2, Stop);
   end;

   { <New_record_type> ::= RECORDX <Field_list> ENDX }
   procedure New_record_type(index : integer; Stop : Token_set);
   var
      LastField : ptr;
   begin
      Match(RECORDX, Stop + [IDX, ENDX]);
      Field_list(LastField, Stop + [ENDX]);
      { Insertamos el record aqui para que no nos esten poniendo
        los campos del mismo tipo que el record }
      insert(index, CL_RECORD_TYPE, obj);
      obj^.LastField := LastField;
      Match(ENDX, Stop);
   end;

   { <Field_list> ::= <Record_section> {; <Record_section>} }
   { LastField es un puntero al ultimo campo }
   procedure Field_list(var LastField : ptr; Stop : Token_set);
   begin
      { Creamos un nuevo bloque e insertamos los campos }
      set_();
        
      Record_section(LastField, Stop + [SEMICOLONX]);
      while(lookahead = SEMICOLONX) do
      begin
         Match(SEMICOLONX, Stop + [IDX, SEMICOLONX]);
         Record_section(LastField, Stop + [SEMICOLONX]);
      end;
      { Salimos del bloque pero sin hacer dispose, sabiendo, que
        al hacer el proximo set, pondra el BlockTable[BlockLevel]
        a NIL y no pasara nada, pero cuidado el unico puntero que
        nos queda a esta memoria es LastField que guardamos en el
        objeto del record... }
      BlockTable[BlockLevel].LastObject := NIL;
      dec(BlockLevel);
   end;

   { <Record_section> ::= IDX {, IDX} : IDX }
   procedure Record_section(var LastField : ptr; 
                            Stop : Token_set);
   var
      num : integer; { Numero de variables de la lista hay que 
                       saberlo para luego ponerles el tipo al 
                       final }
      type_ : ptr;
   begin
      num := 0;
      if(lookahead = IDX) then
      begin
         insert(atrib^.ptr^.Index, CL_FIELD, obj);
         inc(num);
      end
      else
         insert(NO_NAME, CL_FIELD, obj);
      Match(IDX, Stop + [COMMAX, COLONX, IDX]);
      while(lookahead = COMMAX) do
      begin
         Match(COMMAX, Stop + [COMMAX, COLONX, IDX]);
         if(lookahead = IDX) then
         begin
            insert(atrib^.ptr^.Index, CL_FIELD, obj);
            inc(num);
         end
         else
            insert(NO_NAME, CL_FIELD, obj);
         Match(IDX, Stop + [COMMAX, COLONX, IDX]);
      end;
      LastField := obj;
      Match(COLONX, Stop + [IDX]);
      { if(lookahead = IDX) then
         lookup(atrib^.ptr^.Index, obj); }
      { Bajamos y subimos nivel para que al buscar 
        el tipo de dato de los campos no encuentre
        un campo con el mismo nombre }
      dec(BlockLevel);
      Type_Name(type_);
      inc(BlockLevel);
      obj := LastField;
      while((num <> 0) and (obj <> NIL)) do
      begin
         if(obj^.kind <> CL_UNDEFINED) then
         begin
            dec(num);
            obj^.FieldType := type_;
         end;
         obj := obj^.previous;
      end;
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
   var
      nouse : ptr;
   begin
      Variable_group(CL_VARIABLE, nouse, Stop + [SEMICOLONX]);
      Match(SEMICOLONX, Stop);
   end;

   { <Variable_group> ::= IDX {, IDX} COLONX IDX }
   procedure Variable_group(kind : class; var LastVar : ptr; 
                            Stop : Token_set);
   var 
      num : integer;
      type_ : ptr;
   begin
      num := 0;
      if(lookahead = IDX) then
      begin
         insert(atrib^.ptr^.Index, kind, obj);
         inc(num);
      end
      else
         insert(NO_NAME, kind, obj);
      Match(IDX, Stop + [COMMAX, COLONX, IDX]);
      while(lookahead = COMMAX) do
      begin
         Match(COMMAX, Stop + [COMMAX, COLONX, IDX]);
         if(lookahead = IDX) then
         begin
            insert(atrib^.ptr^.Index, kind, obj);
            inc(num);
         end
         else
            insert(NO_NAME, kind, obj);
         Match(IDX, Stop + [COMMAX, COLONX, IDX]);
      end;
      LastVar := obj;
      Match(COLONX, Stop + [IDX]);
      { if(lookahead = IDX) then
         lookup(atrib^.ptr^.Index, obj); }
      Type_Name(type_);
      { obj := LastVar; } { No hace falta ya que 
                            Type_Name tiene un obj
                            local }
      while(num <> 0) do
      begin
         if(obj^.kind <> CL_UNDEFINED) then
         begin
            dec(num); 
            obj^.VarType := type_;
         end;
         obj := obj^.previous;
      end;
      Match(IDX, Stop);
   end;

   { <Procedure_definition> ::= PROCEDUREX IDX 
                               <Procedure_block> ;
   }
   procedure Procedure_definition(Stop : Token_set);
   var
      { LastParam : ptr; } { Puntero al ultimo parametro }
      obj_bak : ptr; { Guardamos obj del procedimiento
                       hasta que sepamos sus parametros }
   begin
      Match(PROCEDUREX, Stop + [IDX, LEFTPARENTHESISX, 
                                SEMICOLONX]);
      if(lookahead = IDX) then
         insert(atrib^.ptr^.Index, CL_PROCEDURE, obj)
      else
         insert(NO_NAME, CL_PROCEDURE, obj);
      obj_bak := obj;
      Match(IDX, Stop + [LEFTPARENTHESISX, SEMICOLONX]);
      { Pasamos el puntero del registro del procedimiento
        para que cuando sepa la lista de parametros 
        establezca el puntero LastParam }
      Procedure_block(obj_bak, Stop + [SEMICOLONX]);
      Match(SEMICOLONX, Stop);
   end;

   { <Procedure_block> ::= [LEFTPARENTHESISX 
                           <Formal_parameter_list> 
                           RIGHTPARENTHESISX] SEMICOLONX 
                           <Block_body>
   }
   procedure Procedure_block(obj_bak : ptr; Stop : Token_set);
   var
      LastParam : ptr;
      saveptr : ptr; { Para salvaguardar la 
                       lista de parametros }
   begin
      LastParam := NIL;
      set_();
      if(lookahead = LEFTPARENTHESISX) then
      begin
         Match(LEFTPARENTHESISX, Stop + [VARX, IDX, 
                                         RIGHTPARENTHESISX,
                                         SEMICOLONX, CONSTX,
                                         TYPEX, PROCEDUREX,
                                         BEGINX]);
         Formal_parameter_list(LastParam, 
                               Stop + [RIGHTPARENTHESISX, 
                                       SEMICOLONX, CONSTX, TYPEX,
                                       VARX, PROCEDUREX, BEGINX]);
         Match(RIGHTPARENTHESISX, Stop + [SEMICOLONX, CONSTX, 
                                          TYPEX, VARX,
                                          PROCEDUREX, BEGINX]);
      end;
      obj_bak^.LastParam := LastParam;
      { Separa los params de lo demas }
      insert(NO_NAME, CL_UNDEFINED, obj); 
      saveptr := obj;
      Match(SEMICOLONX, Stop + [CONSTX, TYPEX, VARX, PROCEDUREX,
                                BEGINX]);
      Block_body(Stop);
      saveptr^.previous := NIL;
      reset_();
   end;

   { <Formal_parameter_list> ::= <Parameter_definition> 
                                 {SEMICOLONX 
                                 <Parameter_definition>}
   }
   procedure Formal_parameter_list(var LastParam : ptr; 
                                   Stop : Token_set);
   begin
      Parameter_definition(LastParam, Stop + [SEMICOLONX]);
      while(lookahead = SEMICOLONX) do
      begin
         Match(SEMICOLONX, Stop + [VARX, IDX, SEMICOLONX]);
         Parameter_definition(LastParam, Stop + [SEMICOLONX]);
      end;
   end;

   { <Parameter_definition> ::= [VARX] <Variable_group> }
   procedure Parameter_definition(var LastParam : ptr; 
                                  Stop : Token_set);
   var
      kind : class;
   begin
      if(lookahead = VARX) then
      begin
         kind := CL_VAR_PARAMETER;
         Match(VARX, Stop + [IDX]);
      end
      else
         kind := CL_VALUE_PARAMETER;
      Variable_group(kind, LastParam, Stop);
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
         { Se le podria pasar obj a Statement2 como variable
           global pero pasandolo asi queda mas elegante
           y hace mas claro el codigo }
         Statement2(obj, Stop);
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
   procedure Statement2(obj : ptr; Stop : Token_set);
   var
      type1, type2 : ptr;
   begin
      { Aqui analizamos, si tiene parametros tiene que ser
        un procedimiento, y en el caso de que no los tenga
        si es un procedimiento hay que ver si es verdad que
        no necesita parametros... Asi que llamamos a 
        Procedure_statement tambien }
      if((lookahead = LEFTPARENTHESISX) or 
         (obj^.kind in [CL_PROCEDURE, CL_STANDARD_PROC])) then
         Procedure_statement(obj, Stop)
      else if(lookahead in [LEFTBRACKETX, PERIODX, 
                            BECOMESX]) then
      begin
         { Cuidado, no estamos seguro de que el obj sea
           una variable, podrian habernos pasado el id
           de una constante }
         if(obj^.kind in [CL_VARIABLE, CL_VALUE_PARAMETER,
                          CL_VAR_PARAMETER]) then
            type1 := obj^.VarType
         else
            TypexError(type1);
         while(lookahead in [LEFTBRACKETX, PERIODX]) do
         begin
            { type1 := obj^.VarType; }
            Selector(type1, 
                     Stop + [BECOMESX, LEFTBRACKETX, PERIODX,
                             PLUSX, MINUSX, NUMERALX, IDX, 
                             LEFTPARENTHESISX, NOTX]);
         end;
         Match(BECOMESX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                                 LEFTPARENTHESISX, NOTX]);
         Expression(type2, Stop);
         { writeln(line_num, ' - ', type2^.name); }
         CheckTypes(type1, type2);
      end
      else
      begin
         { Arreglando el fallo de x; cuando x es integer 
           y tambien erroneo y rana... }
         write_error(SEMANTIC, 'No existe el procedimiento');
         Syntax_check(Stop);
      end;
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

   { IO_Statement }
   procedure IO_Statement(obj : ptr; Stop : Token_set);
   var
      type_ : ptr;
      { Id : ptr; }
      msg : string = 'Se esperaba una variable entera como parametro';
   begin
      if(obj^.name = READ_) then { READ }
      begin
         if(lookahead <> IDX) then
         begin
            write_error(SEMANTIC, msg);
         end
         else
         begin
            type_ := TypeError;
            lookup(atrib^.ptr^.Index, obj);     
            if(obj^.kind in [CL_VALUE_PARAMETER, 
                             CL_VAR_PARAMETER,
                             CL_VARIABLE]) then
               type_ := obj^.VarType
            else
               TypexError(type_);
            Match(IDX, Stop + [LEFTBRACKETX, PERIODX]);
            Factor2(type_, Stop); 
            if(type_ <> TypeInteger) then
               write_error(SEMANTIC, msg); 
         end;
      end
      else                       { WRITE }
      begin
         Expression(type_, Stop + [ENDX]);
         if(type_ <> TypeInteger) then
            write_error(SEMANTIC, msg);
      end;
   end;

   { <Procedure_statement> ::= <IO_Statement> | 
                               [LEFTPARENTHESISX 
                               <Actual_parameter_list> 
                               RIGHTPARENTHESISX] }
   procedure Procedure_statement(obj : ptr; Stop : Token_set);
   begin
      if(lookahead = LEFTPARENTHESISX) then 
      begin
         Match(LEFTPARENTHESISX, Stop + [PLUSX, MINUSX, NUMERALX,
                                         IDX, LEFTPARENTHESISX,
                                         NOTX, 
                                         RIGHTPARENTHESISX]);
         if(obj^.name in [READ_, WRITE_]) then
            IO_Statement(obj, Stop + [RIGHTPARENTHESISX])
         else
         begin
            { Aqui tengo cuidado con que llamen a un proc asi:
              inutil();
              Cuando inutil no lleva parametros...
            }
            if(obj^.LastParam = NIL) then
               write_error(SEMANTIC, 'No puedes llamar asi al procedimiento');
            Actual_parameter_list(obj^.LastParam, 
                                  Stop + [RIGHTPARENTHESISX]);
         end;
         if(lookahead = COMMAX) then
            write_error(SEMANTIC, 'Demasiados parametros');
         Match(RIGHTPARENTHESISX, Stop);
      end
      else 
      { Cuidado que acaban de llamar a un proc sin parametros }
      begin
         if((obj^.LastParam <> NIL) or 
            (obj^.name in [READ_, WRITE_])) then
            write_error(SEMANTIC, 'Faltan parametros');
      end;
   end;

   { <Actual_parameter_list> ::= <Expression>
                                 {COMMAX <Expression>}
   }
   procedure Actual_parameter_list(param : ptr; 
                                   Stop : Token_set);
   var
      type_ : ptr;
   begin
      if(param <> NIL) then
      begin
         Actual_parameter_list(param^.previous, 
                               Stop + [PLUSX, MINUSX, NUMERALX, 
                                       IDX, LEFTPARENTHESISX,
                                       NOTX, COMMAX]);
         if(param^.previous <> NIL) then
         begin
            if(lookahead = RIGHTPARENTHESISX) then
               write_error(SEMANTIC, 'Faltan parametros');
            Match(COMMAX, Stop + [PLUSX, MINUSX, NUMERALX, IDX, 
                                  LEFTPARENTHESISX, NOTX]);
         end;
         { Paso de parametros por variable }
         if(param^.kind = CL_VAR_PARAMETER) then
         begin
            if(lookahead = IDX) then
            begin
               lookup(atrib^.ptr^.Index, obj);  
               type_ := TypeError;
               if(obj^.kind in [CL_VARIABLE, CL_VAR_PARAMETER,
                                CL_VALUE_PARAMETER]) then
                  type_ := obj^.VarType
               else
               begin
               {type_ := TypeError; Ya esta arriba inicializ. }
                  write_error(SEMANTIC, 'Parametros incorrectos 1');
               end;
               Match(IDX, Stop + [LEFTBRACKETX, PERIODX]);
               Factor2(type_, Stop); { while Selector... }
            end
            else
               write_error(SEMANTIC, 'Parametros incorrectos 2');
            if((lookahead <> COMMAX) and 
               (lookahead <> RIGHTPARENTHESISX)) then
               write_error(SEMANTIC, 'Parametros incorrectos 3');
         end
         else 
            Expression(type_, Stop + [COMMAX]);    
         CheckTypes(type_, param^.VarType);
      end;
      {
      Expression(Stop + [COMMAX]);
      while(lookahead = COMMAX) do 
      begin
         Match(COMMAX, Stop + [PLUSX, MINUSX, NUMERALX, IDX, 
                               LEFTPARENTHESISX, NOTX, COMMAX]);
         Expression(Stop + [COMMAX]);
      end;
      }        
   end;

   { <If_statement> ::= IFX <Expression> THENX <Statement>
                        [ELSE <Statement>]
   }
   procedure If_statement(Stop : Token_set);
   var
      type_ : ptr;
   begin
      Match(IFX, Stop + [PLUSX, MINUSX, NUMERALX, IDX, 
                         LEFTPARENTHESISX, NOTX, THENX, IFX,
                         WHILEX, BEGINX, ELSEX]);
      Expression(type_, Stop + [THENX, IDX, IFX, WHILEX, BEGINX, ELSEX]);
      CheckTypes(type_, TypeBoolean);
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
   var
      type_ : ptr;
   begin
      Match(WHILEX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                            LEFTPARENTHESISX, NOTX, DOX, IFX,
                            WHILEX, BEGINX]);
      Expression(type_, Stop + [DOX, IDX, IFX, WHILEX, BEGINX]);
      CheckTypes(type_, TypeBoolean);
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
   procedure Expression(var type_ : ptr; Stop : Token_set);
   var
      type2 : ptr;
   begin
      { Cuidado que en caso de que no haya relational estas
        devolviendo un tipo que podria ser integer, hay que
        tener cuidado en if_statement con esto }
      Simple_expression(type_, Stop + [LESSX, EQUALX, GREATERX, 
                                NOTGREATERX, NOTEQUALX, 
                                NOTLESSX]);
      if(lookahead in [LESSX, EQUALX, GREATERX, NOTGREATERX, 
                       NOTEQUALX, NOTLESSX]) then 
      begin
         Relational_operator(Stop + [PLUSX, MINUSX, NUMERALX,
                                     IDX, LEFTPARENTHESISX, 
                                     NOTX]);
         Simple_expression(type2, Stop);
         CheckTypes(type_, type2);
         if(type_ <> TypeError) then
            type_ := TypeBoolean;
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
   procedure Simple_expression(var type_ : ptr; Stop : Token_set);
   var
      type2 : ptr; 
      is_int : boolean;
   begin
      is_int := false;
      if(lookahead in [PLUSX, MINUSX]) then
      begin
         Sign_operator(Stop + [NUMERALX, IDX, LEFTPARENTHESISX,
                               NOTX, PLUSX, MINUSX, ORX]);
         is_int := true;
      end;
      Term(type_, Stop + [PLUSX, MINUSX, ORX]);
      if(is_int) then
         CheckTypes(type_, TypeInteger);
      while(lookahead in [PLUSX, MINUSX, ORX]) do 
      begin
         Additive_operator(type2, Stop + [NUMERALX, IDX, 
                                   LEFTPARENTHESISX, NOTX, PLUSX,
                                   MINUSX, ORX]);
         CheckTypes(type_, type2);
         Term(type2, Stop + [PLUSX, MINUSX, ORX]);
         CheckTypes(type_, type2);
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
   procedure Additive_operator(var type_ : ptr; 
                               Stop : Token_set);
   begin
      if(lookahead in [PLUSX, MINUSX]) then
         type_ := TypeInteger
      else if(lookahead = ORX) then
         type_ := TypeBoolean;
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
   procedure Term(var type_ : ptr; Stop : Token_set);
   var
      type2 : ptr;
   begin
      Factor(type_, Stop + [ASTERISKX, DIVX, MODX, ANDX]);
      {
      if(lookahead = ANDX) then
         CheckTypes(type_, TypeBoolean)
      else if(lookahead in [ASTERISKX, DIVX, MODX]) then
         CheckTypes(type_, TypeInteger);
      }
      while(lookahead in [ASTERISKX, DIVX, MODX, ANDX]) do 
      begin 
         Multiplying_operator(type2, Stop + [NUMERALX, IDX,
                                      LEFTPARENTHESISX, NOTX,
                                      ASTERISKX, DIVX, MODX, 
                                      ANDX]);
         CheckTypes(type_, type2);
         Factor(type2, Stop + [ASTERISKX, DIVX, MODX, ANDX]);
         CheckTypes(type_, type2);
      end;
   end;

   { <Multiplying_operator> ::= ASTERISKX | DIVX | MODX | ANDX }
   procedure Multiplying_operator(var type_ : ptr; 
                                  Stop : Token_set);
   begin
      if(lookahead in [ASTERISKX, DIVX, MODX]) then
         type_ := TypeInteger
      else if(lookahead = ANDX) then
         type_ := TypeBoolean;
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
   procedure Factor(var type_ : ptr; Stop : Token_set);
   begin
      if(lookahead = NUMERALX) then
      begin
         type_ := TypeInteger;
         Match(NUMERALX, Stop);
      end
      else if(lookahead = IDX) then 
      begin
         lookup(atrib^.ptr^.Index, obj);  
         if(obj^.kind = CL_CONSTANT) then
            type_ := obj^.CS.ConstType        
         else if((obj^.kind = CL_VALUE_PARAMETER) or 
            (obj^.kind = CL_VAR_PARAMETER) or
            (obj^.kind = CL_VARIABLE)) then
            type_ := obj^.VarType
         else
            TypexError(type_);
         Match(IDX, Stop + [LEFTBRACKETX, PERIODX]);
         Factor2(type_, Stop);
      end
      else if(lookahead = LEFTPARENTHESISX) then 
      begin
         Match(LEFTPARENTHESISX, Stop + [PLUSX, MINUSX, NUMERALX,
                                         IDX, LEFTPARENTHESISX,
                                         NOTX, 
                                         RIGHTPARENTHESISX]);
         Expression(type_, Stop + [RIGHTPARENTHESISX]);
         Match(RIGHTPARENTHESISX, Stop);
      end
      else if(lookahead = NOTX) then 
      begin
         Match(NOTX, Stop + [NUMERALX, IDX, LEFTPARENTHESISX, 
                             NOTX]);
         Factor(type_, Stop);
         CheckTypes(type_, TypeBoolean);
      end
      else
         Syntax_error(Stop);
   end;

   { <Factor2> ::= {<Selector>} | Epilon }
   { Tienes que pasarle el VarType de la variable }
   procedure Factor2(var type_ : ptr; Stop : Token_set);
   begin
      while(lookahead in [LEFTBRACKETX, PERIODX]) do
         Selector(type_, Stop + [LEFTBRACKETX, PERIODX]);
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
   procedure Selector(var type_ : ptr; Stop : Token_set);
   begin
      if(lookahead = LEFTBRACKETX) then
         Index_selector(type_, Stop)
      else if(lookahead = PERIODX) then
         Field_selector(type_, Stop)
      else
         Syntax_error(Stop);
   end;

   { <Index_selector> ::= LEFTBRACKETX <Expression> 
                          RIGHTBRACKETX }
   { Recibe el VarType de la variable en type_ }
   procedure Index_selector(var type_ : ptr; Stop : Token_set);
   var
      tipo_elem : ptr;
   begin
      if(type_^.kind <> CL_ARRAY_TYPE) then
         TypexError(type_);
      Match(LEFTBRACKETX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                                  LEFTPARENTHESISX, NOTX,
                                  RIGHTBRACKETX]);
      Expression(tipo_elem, Stop + [RIGHTBRACKETX]);
      if(type_^.AS.IndexType <> tipo_elem) then
         TypexError(type_)
      else
         type_ := type_^.AS.ElementType;
      Match(RIGHTBRACKETX, Stop);
   end;

   { <Field_selector> ::= PERIODX IDX }
   { Recibe VarType de la variable en type_ }
   procedure Field_selector(var type_ : ptr; Stop : Token_set);
   var
      field_ptr : ptr;
      found : boolean;
   begin
      Match(PERIODX, Stop + [IDX]);
      if((lookahead = IDX) and (type_^.kind = CL_RECORD_TYPE)) then 
      begin
         { lookup(atrib^.ptr^.Index, obj); }
         found := false;
         field_ptr := type_^.LastParam; 
         while((field_ptr <> NIL) and (not found)) do
         begin
            { if(field_ptr^.name = obj^.name) then }
            if(field_ptr^.name = atrib^.ptr^.Index) then
               found := true
            else 
               field_ptr := field_ptr^.previous;  
         end;
         if(found) then
            type_ := field_ptr^.FieldType
         else
            TypexError(type_);
      end
      else
         TypexError(type_); { Si hay error sintactico el
                              tipo ya no importa pero 
                              habra que devolver algo... }
      Match(IDX, Stop);
   end;

   { <Constant> ::= NUMERALX | IDX }
   {* Salida: 
    *           value -> devuelve el valor entero de la constante 
    *           type_ -> devuelve puntero al tipo de dato de la 
    *                    constante
    *}
   procedure Constant(var value : integer; var type_ : ptr; 
                      Stop : Token_set);
   begin
      if(lookahead = NUMERALX) then
      begin
         type_ := TypeInteger;
         value := atrib^.value;
         Match(NUMERALX, Stop);
      end
      else if(lookahead = IDX) then
      begin
         lookup(atrib^.ptr^.Index, obj);
         if(obj^.kind = CL_CONSTANT) then
         begin
            type_ := obj^.CS.ConstType;
            value := obj^.CS.ConstValue;
         end
         else
         begin
            KindError(obj); 
            value := 0;
            TypexError(type_); { Pone type_ := TypeError; }
         end;
         Match(IDX, Stop);
      end
      else
      begin
         Syntax_error(Stop);
         value := 0;
         TypexError(type_);
      end;
   end;

begin

end.
