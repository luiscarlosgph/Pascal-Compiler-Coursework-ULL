{ 
   Fichero: ambito.pas                          
   Crea las estructuras de datos necesarias para
   conocer los identificadores declarados en cada 
   bloque del programa y muestra errores cuando
   se utiliza un identificador no declarado,
   cuando se alcanza el maximo nivel de 
   anidamiento, o se declaran varios identificadores
   con el mismo nombre.
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      20/11/2009                         
}

unit ambito;

interface
   uses io, errors, analex, stats;
   const
      NO_NAME = -1;
      INTEGER_ = 1;
      BOOLEAN_ = 2;
      FALSE_ = 3;
      TRUE_ = 4;
      READ_ = 5;
      WRITE_ = 6; 

   type 
      { Clases de identificadores }
      class = (CL_ARRAY_TYPE, CL_CONSTANT, CL_FIELD, CL_PROCEDURE, 
               CL_RECORD_TYPE, CL_STANDARD_PROC, CL_STANDARD_TYPE,
               CL_VALUE_PARAMETER, CL_VAR_PARAMETER, CL_VARIABLE,
               CL_PROGRAM, CL_UNDEFINED);
      ptr = ^ObjectRecord;
      TArrayStruct = record 
                        LowerBound : integer;
                        UpperBound : integer;
                        IndexType : ptr;
                        ElementType : ptr;
                     end;
      TConstantStruct = record
                           ConstValue : integer;
                           ConstType : ptr;
                        end;
      ObjectRecord = record
         name : integer;
         previous : ptr;
         kind : class;
         { Kind Union }
         case class of
            CL_ARRAY_TYPE: (AS : TArrayStruct);
            CL_CONSTANT: (CS : TConstantStruct);
            CL_FIELD: (FieldType : ptr);
            CL_PROCEDURE: (LastParam : ptr);
            CL_RECORD_TYPE: (LastField : ptr);
            CL_VALUE_PARAMETER, CL_VAR_PARAMETER, CL_VARIABLE: (VarType : ptr);
      end;
      BlockRecord = record
                       LastObject : ptr;            
                    end;

      var
         { Pila de punteros a las tablas de simbolos }
         BlockTable : array[1..MAX_LEVEL] of BlockRecord;
         BlockLevel : integer; { Nivel por defecto de un bloque }

   { Funciones }
   procedure search(name : integer; LevelNo : integer; var found : boolean; var obj : ptr);
   procedure insert(name : integer; kind : class; var obj : ptr);
   procedure lookup(name : integer; var obj : ptr);
   procedure set_();
   procedure reset_();
   procedure StandardBlock();

implementation

   {* Procedimiento search 
    * Busca un identificador en la subtabla de simbolos local al bloque en curso de compilacion.
    * 
    * Entrada: name    -> identificador a buscar
    *          LevelNo -> nivel del bloque en el que se lleva a cabo la busqueda
    * Salida:  found   -> indica si se ha encontrado o no, es true si lo encuentra
    *          obj     -> es un puntero a su registro, en caso de que haya sido encontrado
    *}
   procedure search(name : integer; LevelNo : integer; var found : boolean; var obj : ptr);
   begin
      found := false;
      obj := BlockTable[LevelNo].LastObject;  
      while((obj <> NIL) and (not found)) do
      begin
         if(obj^.name = name) then
         begin
            found := true; 
         end
         else
            obj := obj^.previous;
      end;
   end;

   {* Procedimiento insert 
    * Inserta un nuevo identificador en el bloque en curso.
    *
    * Entrada: name -> identificador a insertar
    *          kind -> clase del identificador
    * Salida:  obj  -> puntero a su registro
    *}
   procedure insert(name : integer; kind : class; var obj : ptr);
   var
      usado : boolean;
      msg : string; 
   begin
      { Stats }
      inc(totiden); { Numero de declaraciones de identificadores }
      inc(stacknow[BlockLevel]); { Cuenta del numero de elem. en pila }

      search(name, BlockLevel, usado, obj);
      if(usado) then
      begin
         msg := 'Identificador [' + atrib^.ptr^.lexeme + '] ambiguo';
         write_error(SEMANTIC, msg); 
      end;
      new(obj);
      obj^.name := name;
      obj^.kind := kind;
      obj^.previous := BlockTable[BlockLevel].LastObject;
      BlockTable[BlockLevel].LastObject := obj;
   end;
   
   {* Procedimiento lookup
    * Busca la definicion de un identificador.
    * 
    * Endtrada: name -> identificador
    * Salida:   obj  -> registro donde se encontro
    *                   o se creo si no existia
    *}
   procedure lookup(name : integer; var obj : ptr);
   var
      level : integer; 
      found : boolean;
      msg : string;
   begin
      found := false;
      level := BlockLevel;
      while((level > 0) and (not found))do
      begin
         search(name, level, found, obj);
         dec(level);
      end;
      if(not found) then
      begin
         msg := 'Identificador [' + atrib^.ptr^.lexeme + '] no declarado previamente'; 
         write_error(SEMANTIC, msg);
         insert(name, CL_UNDEFINED, obj); 
      end
      else
         inc(totref);
   end;

   {* Procedimiento set_
    * Inicializa la tabla de simbolos correspondiente a cada nuevo bloque.
    *}
   procedure set_();
   begin
      if(BlockLevel < MAX_LEVEL) then
      begin
         inc(BlockLevel);
         stacknow[BlockLevel] := 0; { Stats de pila mas larga }
         BlockTable[BlockLevel].LastObject := NIL;
      end
      else
         write_error(SEMANTIC, 'Se ha alcanzado el nivel maximo de anidamiento');
   end;
   
   {* Procedimiento reset
    * Salir de un bloque.
    *}
   procedure reset_();
      var p, q : ptr;
   begin
      if(stacknow[BlockLevel] > stacklen) then
         stacklen := stacknow[BlockLevel];
      p := BlockTable[BlockLevel].LastObject;
      BlockTable[BlockLevel].LastObject := NIL;
      while(p <> NIL) do
      begin
         q := p^.previous;
         dispose(p);
         p := q;
      end;
      dec(BlockLevel);
   end;

   {* Procedimiento StandardBlock
    * Inicializa la tabla de punteros a pilas
    * de bloque e inserta los identificadores
    * estandar en el bloque inicial
    *}
   procedure StandardBlock();
   var
      obj : ptr;
   begin
      for BlockLevel := 1 to MAX_LEVEL do
         BlockTable[BlockLevel].LastObject := NIL;
      BlockLevel := 0;
      set_();
      insert(NO_NAME, CL_STANDARD_TYPE, obj);
      insert(INTEGER_, CL_STANDARD_TYPE, obj);
      insert(BOOLEAN_, CL_STANDARD_TYPE, obj);
      insert(FALSE_, CL_CONSTANT, obj);
      insert(TRUE_, CL_CONSTANT, obj);
      insert(READ_, CL_STANDARD_PROC, obj);
      insert(WRITE_, CL_STANDARD_PROC, obj);
   end;

begin

end.
