{ 
   Fichero: syntax.pas                            
   Este fichero lleva incrustados los 
   siguientes modulos conceptuales:
   
   * Analizador sintactico descendente recursivo    
     predictivo para Pascal- con recuperacion de    
     errores.
   
   * Analizador semantico: comprueba el ambito y
     los tipos

   * Generador de codigo intermedio (CI) 
     para una maquina orientada a pila 
     (P-Maquina)
   
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      04/12/2009                         
}

unit syntax;

interface
   uses analex, io, errors, symtable, ambito, types, gencode;

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
      
      { Procedimientos Sintactico - Semantico - Generacion CI }
      procedure Match(tok : Token; Stop : Token_set);
      procedure Syntax_check(Stop : Token_set);
      procedure Syntax_error(Stop : Token_set);
      procedure Program1(Stop : Token_set);                 
      procedure Block_body(var BeginLable, VarLable, 
                               TempLable : integer;
                           Stop : Token_set);
      procedure Constant_definition_part(Stop : Token_set); 
      procedure Constant_definition(Stop : Token_set);      
      procedure Type_definition_part(Stop : Token_set);     
      procedure Type_definition(Stop : Token_set);          
      procedure New_type(index : integer; Stop : Token_set);
      procedure New_array_type(index : integer; 
                               Stop : Token_set);
      procedure Index_range(var val1 : integer; var type1 : ptr; 
                            var val2 : integer; var type2 : ptr;
                            Stop : Token_set);
      procedure New_record_type(index : integer; 
                                Stop : Token_set);
      procedure Field_list(var LastField : ptr; 
                           Stop : Token_set);
      procedure Record_section(var LastField : ptr; 
                               var length_ : integer;
                               Stop : Token_set);
      procedure Variable_definition_part(var length_ : integer;
                                         Stop : Token_set);
      procedure Variable_definition(var LastVar : ptr;
                                    var length_ : integer; 
                                    Stop : Token_set);
      procedure Variable_group(kind : class; var LastVar : ptr; 
                               var typex : ptr; 
                               var number : integer;
                               Stop : Token_set);
      procedure Procedure_definition(Stop : Token_set);     
      procedure Procedure_block(obj_bak : ptr; 
                                var ParamLength : integer;
                                Stop : Token_set);
      procedure Formal_parameter_list(var LastParam : ptr; 
                                      var length_ : integer;
                                      Stop : Token_set);
      procedure Parameter_definition(var LastParam : ptr; 
                                     var length_ : integer;
                                     Stop : Token_set);
      procedure IO_Statement(obj : ptr; Stop : Token_set);
      procedure Statement(Stop : Token_set);                
      procedure Statement2(obj : ptr; Stop : Token_set);
      { procedure Assignment_statement(Stop : Token_set); }     
      procedure Procedure_statement(obj : ptr; Stop : Token_set);
      procedure Actual_parameter_list(param : ptr; 
                                   var ParamLength : integer;
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
   var
      VarLable, TempLable, BeginLable : integer; { Para GC }
   begin
      set_(); { No permito ids iguales al nombre del programa } 
      Match(PROGRAMX, Stop + [IDX, SEMICOLONX, CONSTX, TYPEX, 
                             VARX, PROCEDUREX, BEGINX, PERIODX]);
      if(lookahead = IDX) then
         insert(atrib^.ptr^.Index, CL_PROGRAM, obj)
      else
         insert(NO_NAME, CL_PROGRAM,obj);
      { GC }
      new_lable(VarLable);
      new_lable(TempLable);
      new_lable(BeginLable);
      emit5(PROGRAM_, VarLable, TempLable, BeginLable, 
            line_num);
      { Fin GC }
      { set_(); Si permito ids iguales al nombre del programa }
      Match(IDX, Stop + [SEMICOLONX, CONSTX, TYPEX, VARX, 
                         PROCEDUREX, BEGINX, PERIODX]);
      Match(SEMICOLONX, Stop + [CONSTX, TYPEX, VARX, PROCEDUREX, 
                                BEGINX, PERIODX]);
      Block_body(BeginLable, VarLable, TempLable, 
                 Stop + [PERIODX]);
      Match(PERIODX, Stop);
      reset_();
      { GC }
      emit1(ENDPROG_);
      { Fin GC }
   end;
   
   { <Block_body> ::= [<Constant_definition_part>]
                      [<Type_definition_part>]
                      [<Variable_definition_part>]
                      {<Procedure_definition>}
                      <Compound_statement>
   }
   procedure Block_body(var BeginLable, VarLable, 
                            TempLable : integer;
                        Stop : Token_set);
   var
      length_ : integer; { GC - Lo que ocupan las var locales } 
   begin
      { TODO Syntax_check( ... ); } 
      if(lookahead = CONSTX) then
         Constant_definition_part(Stop + [TYPEX, VARX, 
                                          PROCEDUREX, BEGINX]);
      if(lookahead = TYPEX) then
         Type_definition_part(Stop + [VARX, PROCEDUREX, BEGINX]);
      if (lookahead = VARX) then
         Variable_definition_part(length_, 
                                  Stop + [PROCEDUREX, BEGINX])
      else
         length_ := 0; { GC }
      while (lookahead = PROCEDUREX) do
         Procedure_definition(Stop + [PROCEDUREX, BEGINX]);
      { GC }
      emit2(DEFADDR_, BeginLable); 
      { Fin GC }
      Compound_statement(Stop);
      { GC }
      emit3(DEFARG_, VarLable, length_);
      emit3(DEFARG_, TempLable, BlockTable[BlockLevel].MaxTemp);
      { Fin GC }
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
      val1, val2 : integer; { Cota inferior y superior del 
                              array }
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
   var
      more, length_ : integer;
   begin
      { Creamos un nuevo bloque e insertamos los campos }
      set_();
        
      Record_section(LastField, length_, Stop + [SEMICOLONX]);
      while(lookahead = SEMICOLONX) do
      begin
         Match(SEMICOLONX, Stop + [IDX, SEMICOLONX]);
         Record_section(LastField, more, Stop + [SEMICOLONX]);
         length_ := length_ + more;
      end;
      field_addressing(length_, LastField);
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
                            var length_ : integer;
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
      { Generacion de codigo }
      length_ := num * type_length(type_);
      { Fin GC }
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
   procedure Variable_definition_part(var length_ : integer;
                                      Stop : Token_set);
   var
      LastVar : ptr;
      more : integer;
   begin
      Match(VARX, Stop + [IDX]);
      Variable_definition(LastVar, length_, Stop + [IDX]);
      while(lookahead = IDX) do
      begin
         Variable_definition(LastVar, more, Stop + [IDX]);
         length_ := length_ + more;
      end;
      variable_addressing(length_, LastVar);
   end;

   { <Variable_definition> ::= <Variable_group> SEMICOLONX }
   procedure Variable_definition(var LastVar : ptr;
                                 var length_ : integer; 
                                 Stop : Token_set);
   var
      typex : ptr; { Tipo del grupo de variables }
      number : integer; { Numero de variables del grupo }
   begin
      Variable_group(CL_VARIABLE, LastVar, typex, number, 
                     Stop + [SEMICOLONX]);
      length_ := number * type_length(typex);
      Match(SEMICOLONX, Stop);
   end;

   { <Variable_group> ::= IDX {, IDX} COLONX IDX }
   procedure Variable_group(kind : class; var LastVar : ptr; 
                            var typex : ptr; 
                            var number : integer;
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
      number := num; { Devolvemos el numero de params }
      typex := type_; { Devolvemos el tipo del grupo } 
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
      ParamLength : integer; { Palabras que ocupan los params }
   begin
      Match(PROCEDUREX, Stop + [IDX, LEFTPARENTHESISX, 
                                SEMICOLONX]);
      if(lookahead = IDX) then
         insert(atrib^.ptr^.Index, CL_PROCEDURE, obj)
      else
         insert(NO_NAME, CL_PROCEDURE, obj);
      { GC }
      obj^.ProcLevel := BlockLevel;
      new_lable(obj^.ProcLable);
      { Fin GC }
      obj_bak := obj;
      Match(IDX, Stop + [LEFTPARENTHESISX, SEMICOLONX]);
      { Pasamos el puntero del registro del procedimiento
        para que cuando sepa la lista de parametros 
        establezca el puntero LastParam }
      Procedure_block(obj_bak, ParamLength, Stop + [SEMICOLONX]);
      Match(SEMICOLONX, Stop);
      { GC }
      emit2(ENDPROC_, ParamLength); 
      { Fin GC }
   end;

   { <Procedure_block> ::= [LEFTPARENTHESISX 
                           <Formal_parameter_list> 
                           RIGHTPARENTHESISX] SEMICOLONX 
                           <Block_body>
   }
   procedure Procedure_block(obj_bak : ptr; 
                             var ParamLength : integer;
                             Stop : Token_set);
   var
      LastParam : ptr; { Formal_parameter_list nos guarda aqui
                         un puntero al ultimo de la lista de 
                         parametros, despues se lo asignamos al
                         obj que nos pasaron }
      saveptr : ptr; { Para salvaguardar la 
                       lista de parametros }
      VarLable, TempLable, BeginLable : integer; 
   begin
      { Si no hay parametros nos curamos en salud con estas dos
        sentencias }
      LastParam := NIL;
      ParamLength := 0;
      set_();
      if(lookahead = LEFTPARENTHESISX) then
      begin
         Match(LEFTPARENTHESISX, Stop + [VARX, IDX, 
                                         RIGHTPARENTHESISX,
                                         SEMICOLONX, CONSTX,
                                         TYPEX, PROCEDUREX,
                                         BEGINX]);
         Formal_parameter_list(LastParam, ParamLength, 
                               Stop + [RIGHTPARENTHESISX, 
                                      SEMICOLONX, CONSTX, TYPEX,
                                      VARX, PROCEDUREX, BEGINX]);
         Match(RIGHTPARENTHESISX, Stop + [SEMICOLONX, CONSTX, 
                                          TYPEX, VARX,
                                          PROCEDUREX, BEGINX]);
      end;
      obj_bak^.LastParam := LastParam;
      { GC }
      new_lable(VarLable);
      new_lable(TempLable);
      new_lable(BeginLable);
      emit2(DEFADDR_, obj_bak^.ProcLable);
      emit5(PROCEDURE_, VarLable, TempLable, BeginLable, 
            line_num);
      { Fin GC }
      { Separa los params de lo demas, ocurre que como el
        procedimiento esta insertado en el bloque anterior
        y los procedimientos en este bloque al hacer reset nos
        cargamos los parametros formales y eso es una chapuza }
      insert(NO_NAME, CL_UNDEFINED, obj); 
      saveptr := obj;
      Match(SEMICOLONX, Stop + [CONSTX, TYPEX, VARX, PROCEDUREX,
                                BEGINX]);
      Block_body(BeginLable, VarLable, TempLable, 
                 Stop + [SEMICOLONX]);
      saveptr^.previous := NIL; { Esto evita que el reset se
                                  cargue los parametros form. }
      reset_();
   end;

   { <Formal_parameter_list> ::= <Parameter_definition> 
                                 {SEMICOLONX 
                                 <Parameter_definition>}
   }
   procedure Formal_parameter_list(var LastParam : ptr; 
                                   var length_ : integer;
                                   Stop : Token_set);
   var
      more : integer;
   begin
      Parameter_definition(LastParam, length_,
                           Stop + [SEMICOLONX]);
      while(lookahead = SEMICOLONX) do
      begin
         Match(SEMICOLONX, Stop + [VARX, IDX, SEMICOLONX]);
         Parameter_definition(LastParam, more, 
                              Stop + [SEMICOLONX]);
         length_ := length_ + more;
      end;
      parameter_addressing(length_, LastParam);
   end;

   { <Parameter_definition> ::= [VARX] <Variable_group> }
   procedure Parameter_definition(var LastParam : ptr; 
                                  var length_ : integer;
                                  Stop : Token_set);
   var
      kind : class;
      typex : ptr; { Tipo del grupo de parametros }
      number : integer; { Numero de params del grupo }
   begin
      if(lookahead = VARX) then
      begin
         kind := CL_VAR_PARAMETER;
         Match(VARX, Stop + [IDX]);
      end
      else
         kind := CL_VALUE_PARAMETER;
      Variable_group(kind, LastParam, typex, number, Stop);
      length_ := number * type_length(typex);
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
      length_ : integer; { Para GC }
      level : integer;
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
         { GC }
         level := BlockLevel - obj^.VarLevel;  
         if(obj^.kind = CL_VAR_PARAMETER) then
            emit3(VARPARAM_, level, obj^.VarDispl)
         else
            emit3(VARIABLE_, level, obj^.VarDispl);
         push(1);
         { Fin GC }
         Match(BECOMESX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                                 LEFTPARENTHESISX, NOTX]);
         Expression(type2, Stop);
         { writeln(line_num, ' - ', type2^.name); }
         CheckTypes(type1, type2);
         { GC de asignacion }
         length_ := type_length(type2); 
         emit2(ASSIGN_, length_);
         pop(1 + length_);
         { Fin GC }
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
            { GC }
            emit1(READ_OP);
            pop(1);
            { Fin GC }
         end;
      end
      else                       { WRITE }
      begin
         Expression(type_, Stop + [ENDX]);
         { GC }
         emit1(WRITE_OP);
         pop(1);
         { Fin GC }
         if(type_ <> TypeInteger) then
            write_error(SEMANTIC, msg);
      end;
   end;

   { <Procedure_statement> ::= <IO_Statement> | 
                               [LEFTPARENTHESISX 
                               <Actual_parameter_list> 
                               RIGHTPARENTHESISX] }
   procedure Procedure_statement(obj : ptr; Stop : Token_set);
   var
      ParamLength : integer; { Para GC }
   begin
      { GC }
      ParamLength := 0;
      { Fin GC }
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
            Actual_parameter_list(obj^.LastParam, ParamLength, 
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
      { GC }
      emit3(PROCCALL_, BlockLevel - obj^.ProcLevel, 
            obj^.ProcLable); 
      push(3);
      pop(ParamLength + 3);
      { Fin GC }
      Syntax_check(Stop);
   end;

   { <Actual_parameter_list> ::= <Expression>
                                 {COMMAX <Expression>}
   }
   procedure Actual_parameter_list(param : ptr; 
                                   var ParamLength : integer;
                                   Stop : Token_set);
   var
      type_ : ptr;
      level : integer; 
   begin
      if(param <> NIL) then
      begin
         Actual_parameter_list(param^.previous, ParamLength,
                               Stop + [COMMAX]);
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
               { GC }
               level := BlockLevel - obj^.VarLevel;  
               if(obj^.kind = CL_VAR_PARAMETER) then
                  emit3(VARPARAM_, level, obj^.VarDispl)
               else
                  emit3(VARIABLE_, level, obj^.VarDispl);
               push(1); 
               { Fin GC }
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
      lable1, lable2 : integer;
   begin
      Match(IFX, Stop + [PLUSX, MINUSX, NUMERALX, IDX, 
                         LEFTPARENTHESISX, NOTX, THENX, IFX,
                         WHILEX, BEGINX, ELSEX]);
      Expression(type_, Stop + [THENX, IDX, IFX, WHILEX, BEGINX, ELSEX]);
      CheckTypes(type_, TypeBoolean); 
      Match(THENX, Stop + [IDX, IFX, WHILEX, BEGINX, ELSEX]);
      { GC }
      new_lable(lable1);
      emit2(GO_FALSE_, lable1);
      { Fin GC }
      Statement(Stop + [ELSEX, IDX, IFX, WHILEX, BEGINX]);
      Syntax_check(Stop + [ELSEX]);
      if(lookahead = ELSEX) then 
      begin
         Match(ELSEX, Stop + [IDX, IFX, WHILEX, BEGINX]);
         { GC }
         new_lable(lable2); 
         emit2(GOTO_, lable2);
         emit2(DEFADDR_, lable1);
         { Fin GC }
         Statement(Stop);
         { GC }
         emit2(DEFADDR_, lable2);
         { Fin GC }
      end
      else { GC }
         emit2(DEFADDR_, lable1); 
           { Fin GC }
   end;

   { <While_statement> ::= WHILEX <Expression> DOX <Statement> }
   procedure While_statement(Stop : Token_set);
   var
      type_ : ptr;
      lable1, lable2 : integer;
   begin
      { GC }
      new_lable(lable1); 
      emit2(DEFADDR_, lable1);
      { Fin GC }
      Match(WHILEX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                            LEFTPARENTHESISX, NOTX, DOX, IFX,
                            WHILEX, BEGINX]);
      Expression(type_, Stop + [DOX, IDX, IFX, WHILEX, BEGINX]);
      CheckTypes(type_, TypeBoolean);
      Match(DOX, Stop + [IDX, IFX, WHILEX, BEGINX]);
      { GC }
      new_lable(lable2);
      emit2(GO_FALSE_, lable2);
      pop(1);
      { Fin GC }
      Statement(Stop);
      { GC }
      emit2(GOTO_, lable1);
      emit2(DEFADDR_, lable2);
      { Fin GC }
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
      operator_ : Token;
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
         operator_ := lookahead;
         Relational_operator(Stop + [PLUSX, MINUSX, NUMERALX,
                                     IDX, LEFTPARENTHESISX, 
                                     NOTX]);
         Simple_expression(type2, Stop);
         { Si los tipos que se comparan son iguales devolvemos
           un tipo booleano porque todo_ fue perfecto }
         CheckTypes(type_, type2);
         if(type_ <> TypeError) then
            type_ := TypeBoolean;
         { Generacion de codigo de la comparacion }
         case operator_ of
            LESSX: emit1(LESS_); 
            EQUALX: emit1(EQUAL_);
            GREATERX: emit1(GREATER_);
            NOTGREATERX: emit1(NOTGREATER_);
            NOTEQUALX: emit1(NOTEQUAL_);
            NOTLESSX: emit1(NOTLESS_);
         end;
         pop(1);
         { Fin de GC }
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
      operator_ : Token;
   begin
      is_int := false;
      if(lookahead in [PLUSX, MINUSX]) then
      begin
         operator_ := lookahead;
         Sign_operator(Stop + [NUMERALX, IDX, LEFTPARENTHESISX,
                               NOTX, PLUSX, MINUSX, ORX]);
         is_int := true;
      end;
      Term(type_, Stop + [PLUSX, MINUSX, ORX]); 
      if(is_int) then
         CheckTypes(type_, TypeInteger);
      { Generacion de codigo - operador de signo }
      if(operator_ = MINUSX) then
         emit1(MINUS_); 
      { Fin GC }
      while(lookahead in [PLUSX, MINUSX, ORX]) do 
      begin
         operator_ := lookahead;
         Additive_operator(type2, Stop + [NUMERALX, IDX, 
                                   LEFTPARENTHESISX, NOTX, PLUSX,
                                   MINUSX, ORX]);
         CheckTypes(type_, type2);
         Term(type2, Stop + [PLUSX, MINUSX, ORX]);
         CheckTypes(type_, type2);
         { Generacion de codigo de la operacion }
         case operator_ of
            PLUSX: emit1(ADD_);
            MINUSX: emit1(SUBSTRACT_);
            ORX: emit1(OR_);
         end;
         pop(1);  
         { Fin GC }
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
      operator_ : Token; { Guardamos el operador para GC }
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
         operator_ := lookahead;
         Multiplying_operator(type2, Stop + [NUMERALX, IDX,
                                      LEFTPARENTHESISX, NOTX,
                                      ASTERISKX, DIVX, MODX, 
                                      ANDX]);
         CheckTypes(type_, type2);
         Factor(type2, Stop + [ASTERISKX, DIVX, MODX, ANDX]);
         CheckTypes(type_, type2);
         { Generacion de codigo de la operacion }
         case operator_ of
            ASTERISKX: emit1(MULTIPLY_);
            DIVX: emit2(DIVIDE_, line_num);
            MODX: emit2(MODULO_, line_num);
            ANDX: emit1(AND_); 
         end;
         pop(1);
         { Fin GC }
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
   var
      level : integer; { Nivel de la variable en caso de que el
                         factor sea un IDX }
      length_ : integer; 
      value : integer; { Valor si el ID es una constante }
   begin
      if(lookahead = NUMERALX) then
      begin
         Constant(value, type_, Stop);
         { GC }
         emit2(CONSTANT_, value);
         push(1);
         { Fin GC }
      end
      else if(lookahead = IDX) then 
      begin
         lookup(atrib^.ptr^.Index, obj);
         if(obj^.kind = CL_CONSTANT) then
         begin
            { type_ := obj^.CS.ConstType }       
            Constant(value, type_, Stop);
            { GC }
            emit2(CONSTANT_, value);
            push(1);
            { Fin GC }
         end
         else if(obj^.kind in [CL_VALUE_PARAMETER, 
                               CL_VAR_PARAMETER, 
                               CL_VARIABLE]) then
         begin
            lookup(atrib^.ptr^.Index, obj);  
            type_ := obj^.VarType;
            { Generacion de codigo de acceso a variable }
            level := BlockLevel - obj^.VarLevel;  
            if(obj^.kind = CL_VAR_PARAMETER) then
               emit3(VARPARAM_, level, obj^.VarDispl)
            else
               emit3(VARIABLE_, level, obj^.VarDispl);
            push(1);
            { Fin GC }
            Match(IDX, Stop + [LEFTBRACKETX, PERIODX]);
            Factor2(type_, Stop);
            { Generacion de codigo - VALUE }
            length_ := type_length(type_); 
            emit2(VALUE_, length_);
            push(length_ - 1);
            { Fin GC }
         end
         else
            TypexError(type_);
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
      length_ : integer;
   begin
      if(type_^.kind <> CL_ARRAY_TYPE) then
         TypexError(type_);
      Match(LEFTBRACKETX, Stop + [PLUSX, MINUSX, NUMERALX, IDX,
                                  LEFTPARENTHESISX, NOTX,
                                  RIGHTBRACKETX]);
      Expression(tipo_elem, Stop + [RIGHTBRACKETX]);
      { Generacion de codigo }
      length_ := type_length(type_^.AS.ElementType);
      emit5(INDEX_, type_^.AS.LowerBound, type_^.AS.UpperBound, 
            length_, line_num);
      { Fin GC }
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
         begin
            type_ := field_ptr^.FieldType;
            { Generacion de codigo }
            emit2(FIELD_, field_ptr^.FieldDispl);
            { Fin GC }
         end
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
