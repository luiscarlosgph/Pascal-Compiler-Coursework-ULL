{
   Fichero: gencode.pas
   Funciones relativas al generador de codigo intermedio (CI)
   para una maquina orientada a pila (P-Maquina), contiene
   tanto las funciones para emitir P-Code como para el
   ensamblador que define los valores de las etiquetas

   Asignatura: Compiladores (ULL)
   Autor:      Luis Carlos Garcia-Peraza Herrera
   Fecha:      19/12/2009
}

unit gencode;

interface
   uses ambito, errors, asm_pcode;

   const
      MAX_INST = 32; { Numero maximo de instrucciones que tiene
                       la P-Maquina, comenzando en 0 }
      MAX_INST_LEN = 12; { Numero maximo de caracteres que puede
                           tener la instruccion en ensamblador }

   type 
      OperationPart = (ADD_, AND_, ASSIGN_, CONSTANT_, DIVIDE_,
                       ENDPROC_, ENDPROG_, EQUAL_, FIELD_, 
                       GO_FALSE_, GOTO_, GREATER_, INDEX_, LESS_,
                       MINUS_, MODULO_, MULTIPLY_, NOT_, 
                       NOTEQUAL_, NOTGREATER_, NOTLESS_, OR_,
                       PROCCALL_, PROCEDURE_, PROGRAM_, 
                       SUBSTRACT_, VALUE_, VARIABLE_, VARPARAM_,
                       READ_OP, WRITE_OP, DEFADDR_, DEFARG_, 
                       ASSEMBLING); 

   var
      tableNames : array[0..MAX_INST] of string[MAX_INST_LEN] = 
         ('ADD', 'AND', 'ASSIGN', 'CONSTANT', 'DIVIDE', 
          'END_PROC', 'END_PROG', 'EQUAL', 'FIELD', 'GO_FALSE', 
          'GOTO', 'GREATER', 'INDEX', 'LESS', 'MINUS', 'MODULO', 
          'MULTIPLY', 'NOT', 'NOT_EQUAL', 'NOT_GREATER', 
          'NOT_LESS', 'OR', 'PROC_CALL', 'PROCEDURE', 'PROGRAM', 
          'SUBSTRACT', 'VALUE', 'VARIABLE', 'VARPARAM', 'READ', 
          'WRITE', 'DEF_ADDR', 'DEF_ARG');
      { Numero de argumentos de cada instruccion, incluyendo como
        argumento a la propia instruccion }
      tableArgs : array[0..MAX_INST] of integer = 
         (1, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 1, 5, 1, 1, 2, 1,
          1, 1, 1, 1, 1, 3, 5, 5, 1, 2, 3, 3, 1, 1, 2, 3); 
      lable_no : integer;    { Numero de etiquetas creadas }
   
   { Procedimientos de generacion de CI }
   procedure init_gencode();
   procedure finish_gencode();
   procedure emit1(op : OperationPart);
   procedure emit2(op : OperationPart; arg : integer);
   procedure emit3(op : OperationPart; arg1, arg2 : integer);
   procedure emit5(op : OperationPart; 
                   arg1, arg2, arg3, arg4 : integer);
   function type_length(typex : ptr) : integer;
   procedure variable_addressing(var_length : integer;
                                 LastVar : ptr);
   procedure parameter_addressing(param_length : integer;
                                  last_param : ptr);
   procedure field_addressing(record_length : integer; 
                              last_field : ptr);
   procedure push(length_ : integer);
   procedure pop(length_ : integer);
   procedure new_lable(var No : integer);

implementation
   { uses }

   {* Procedimiento init_gencode
    * Inicializa las variables de generacion de codigo
    *}
   procedure init_gencode();
   begin
      temp_name := 'temp.$$$'; 
      code_name := 'a.out';
      list_name := 'a.inf'; { Al ensamblar cambialo a a.asm }
      assign(temp, temp_name);
      rewrite(temp);
      assign(code, code_name);
      rewrite(code);
      assign(list, list_name);
      rewrite(list);
      AsmBool := false;
      Emitting := false;
      Address := 0;
      lable_no := -1;
   end;

   {* Procedimiento finish_gencode
    * Cierra los ficheros pendientes 
    *}
   procedure finish_gencode();
   begin
      close(temp);
      close(code);
      close(list);
   end;

   {* Procedimiento emit1
    * Inserta instrucciones en CI sin argumentos 
    *}
   procedure emit1(op : OperationPart);
   begin
      if(AsmBool) then
      begin
         if(Emitting) then
         begin
            writeln(code, ord(op));  
            writeln(list, Address:6, ':', 
                    tableNames[ShortInt(op)]:12);
         end;
         inc(Address);
      end
      else
      begin
         writeln(temp, ord(op));
         writeln(list, tableNames[ShortInt(op)]:12);
      end;
   end;
   
   {* Procedimiento emit2
    * Inserta instruccion en CI con 1 argumento 
    *}
   procedure emit2(op : OperationPart; arg : integer);
   begin
      if(AsmBool) then
      begin
         if(Emitting) then
         begin
            writeln(code, ord(op));
            writeln(code, arg);
            writeln(list, Address:6, ':', 
                    tableNames[ShortInt(op)]:12, arg:6);
         end;
         Address := Address + 2;
      end
      else
      begin
         writeln(temp, ord(op));
         writeln(temp, arg);
         writeln(list, tableNames[ShortInt(op)]:12, arg:6);
      end;
   end;

   {* Procedimiento emit3
    * Inserta instruccion en CI con 2 argumentos 
    *}
   procedure emit3(op : OperationPart; arg1, arg2 : integer);
   begin
      if(AsmBool) then
      begin
         if(Emitting) then
         begin
            writeln(code, ord(op));
            writeln(code, arg1);
            writeln(code, arg2);
            writeln(list, Address:6, ':',   { ASM } 
                    tableNames[ShortInt(op)]:12, arg1:6, arg2:6);
         end;
         Address := Address + 3;
      end
      else
      begin
         writeln(temp, ord(op));
         writeln(temp, arg1);
         writeln(temp, arg2);
         writeln(list, tableNames[ShortInt(op)]:12, arg1:6, {INF} 
                 arg2:6);
      end;
   end;
 
   {* Procedimiento emit5
    * Inserta instruccion en CI con 4 argumentos 
    *}
   procedure emit5(op : OperationPart; 
                   arg1, arg2, arg3, arg4 : integer);
   begin
      if(AsmBool) then
      begin
         if(Emitting) then
         begin
            writeln(code, ord(op));
            writeln(code, arg1);
            writeln(code, arg2);
            writeln(code, arg3);
            writeln(code, arg4);
            writeln(list, Address:6, ':', 
                    tableNames[ShortInt(op)]:12, arg1:6, arg2:6, 
                    arg3:6, arg4:6);
         end;
         Address := Address + 5;
      end
      else
      begin
         writeln(temp, ord(op));
         writeln(temp, arg1);
         writeln(temp, arg2);
         writeln(temp, arg3);
         writeln(temp, arg4);
         writeln(list, tableNames[ShortInt(op)]:12, arg1:6, 
                 arg2:6, arg3:6, arg4:6);
      end;
   end;

   {* Funcion type_length
    * Calcula el tamanyo de una variable 
    *}
   function type_length(typex : ptr) : integer;
   begin
      if(typex^.kind  = CL_STANDARD_TYPE) then
         type_length := 1
      else
      begin
         if(typex^.kind = CL_ARRAY_TYPE) then
            type_length := (typex^.AS.UpperBound - 
                            typex^.AS.LowerBound + 1) * 
                            type_length(typex^.AS.ElementType)
         else { typex^.kind = CL_RECORD_TYPE }
            type_length := typex^.RecordLength;  
      end;
   end;
   
   {* Procedimiento variable_addressing
    * Se mueve a traves de la lista de variables en orden
    * inverso y les asigna a cada una un desplazamiento
    *}
   procedure variable_addressing(var_length : integer;
                                 LastVar : ptr);
   var
      displ : integer;
   begin
      displ := 3 + var_length;
      while(displ > 3) do
      begin
         displ := displ - type_length(LastVar^.VarType); 
         LastVar^.VarLevel := BlockLevel;
         LastVar^.VarDispl := displ;
         LastVar := LastVar^.previous;
      end;
   end;

   {* Procedimiento parameter_addressing
    * Se mueve a traves de la lista de parametros en orden
    * inverso y les asigna a cada una su desplazamiento
    *}
   procedure parameter_addressing(param_length : integer;
                                  last_param : ptr);
   var
      displ : integer;
   begin
      displ := 0;
      while(displ > -param_length) do
      begin
         if(last_param^.kind = CL_VAR_PARAMETER) then
            dec(displ)
         else { last_param^.kind = CL_VALUE_PARAMETER }
            displ := displ - type_length(last_param^.VarType);
         last_param^.VarLevel := BlockLevel;
         last_param^.VarDispl := displ;
         last_param := last_param^.previous;
      end;   
   end;
   
   {* Procedimiento field_addressing
    * A los campos de un tipo se les asignan desplazamientos
    * consecutivos empezando desde cero
    *}
   procedure field_addressing(record_length : integer; 
                              last_field : ptr);
   var
      displ : integer;
   begin
      displ := record_length; 
      while(displ > 0) do
      begin
         displ := displ - type_length(last_field^.FieldType);
         last_field^.FieldDispl := displ;
         last_field := last_field^.previous;
      end;
   end;
   
   {* Procedimiento push
    * Incrementa el campo temp_length del bloque actual y cambia
    * Max_temp si fuese necesario
    *}
   procedure push(length_ : integer);
   begin
      write('TempLength = ', BlockTable[BlockLevel].TempLength);
      BlockTable[BlockLevel].TempLength := 
         BlockTable[BlockLevel].TempLength + length_;
      if(BlockTable[BlockLevel].MaxTemp < 
         BlockTable[BlockLevel].TempLength) then 
         BlockTable[BlockLevel].MaxTemp := 
            BlockTable[BlockLevel].TempLength;
      writeln(', push ', length_);
   end;

   {* Procedimiento pop
    * Decrementa el TempLength correspondiente al bloque actual
    *}
   procedure pop(length_ : integer);
   begin
      write('TempLength = ', BlockTable[BlockLevel].TempLength);
      BlockTable[BlockLevel].TempLength := 
         BlockTable[BlockLevel].TempLength - length_;
      writeln(', pop ', length_);
   end;

   {* Procedimiento new_lable
    * Incrementa el numero de etiquetas porque necesitamos otra
    *}
   procedure new_lable(var No : integer);
   begin
      if(lable_no > max_lable) then
         write_error(SEMANTIC, 'Programa muy largo'); 
      inc(lable_no);
      No := lable_no;
   end;

begin
end.
