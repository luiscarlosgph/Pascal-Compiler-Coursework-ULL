{ 
   Fichero: types.pas                          
   Rutinas dedeicadas especificamente al analisis
   de tipos en el compilador de Pascal-
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      04/12/2009                         
}

unit types;

interface
   uses ambito, analex, errors;

   { Funciones }
   function TypeS(type_ : class) : boolean;
   procedure Type_Name(var type_ : ptr);
   procedure CheckTypes(type1 : ptr; type2 : ptr);
   procedure TypexError(var type_ : ptr);

implementation
   uses syntax;

   {* Funcion TypeS
    * Devuelve true si la clase pasada como parametro
    * es un identificador de tipo, false en caso contrario
    *} 
   function TypeS(type_ : class) : boolean;
   begin
      if((type_ = CL_STANDARD_TYPE) or (type_ = CL_ARRAY_TYPE) or
         (type_ = CL_RECORD_TYPE)) then
         TypeS := true
      else
         TypeS := false;
   end;
   
   {* Procedimiento Type_Name 
    * Devuelve un puntero al identificador de tipo que
    * que se corresponde con el ID presente en lookahead
    * Entrada: Stop -> conjunto de parada [QUITADO]
    * Salida: type_ guardara el puntero al objeto de tipo
    *}
   procedure Type_Name(var type_ : ptr);
   var
      obj : ptr;
   begin
      if(lookahead = IDX) then
      begin
         lookup(atrib^.ptr^.Index, obj); { Busca el id }
         if(TypeS(obj^.kind)) then { Si es id de tipo }
            type_ := obj
         else
         begin
            KindError(obj);
            type_ := TypeError;
         end;
      end
      else
         type_ := TypeError;
      { Match(IDX, Stop + [ENDX]); ??? }
   end;
   
   {* Procedimiento CheckTypes 
    * Comprueba que dos tipos sean iguales, si no lo son
    * muestra un error, se supone que los dos punteros que se le
    * pasan se refieren al objeto del bloque estandar que 
    * almacena el tipo de dato
    *}
   procedure CheckTypes(type1 : ptr; type2 : ptr);
   var
      msg : string;
   begin
      if(type1 <> type2) then
      begin
         if((type1 <> TypeError) and (type2 <> TypeError)) then
         begin
            msg := 'Tipo no valido';
            write_error(SEMANTIC, msg);
         end;
         type1 := TypeError;
      end;
   end;
   
   {* Procedimiento TypexError
    * Muestra un error de tipos teniendo en cuenta que no
    * hubiera otra ya antes
    *}
   procedure TypexError(var type_ : ptr);
   var
      msg : string;
   begin
      if(type_ <> TypeError) then
      begin
         msg := 'Tipo no valido';
         write_error(SEMANTIC, msg);
         type_ := TypeError;
      end;
   end;

begin
end.
