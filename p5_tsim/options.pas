{ 
   Fichero: options.pas                           
   Gestiona las opciones de linea de comandos del 
   programa.
                                                  
   Asignatura: Compiladores (ULL)                 
   Autor:      Luis Carlos Garcia-Peraza Herrera  
   Fecha:      20/11/2009                         
}

unit options;

interface
   uses getopts, io;

   const
      MAXOPTS = 2;

   { type }

   var
      c : char;
      optionindex : longint;
      theopts : array[1..MAXOPTS] of TOption; 
      opts_ok : word; { Indica si hubo error al
                        parsear linea de comandos }
      
      { Opciones del programa }
      verbose : word;

   procedure parse_opts();
   procedure print_usage();

implementation
   
   { Parsea las opciones de la linea de comandos 
     y establece las variables:
        verbose -> debug
   }
   procedure parse_opts(); 
   begin
      if not(paramcount > 0) then
      begin
         writeln();
         writeln('   (ERROR) Introduce un fichero de codigo fuente.');
         print_usage();
      end;
      verbose := 0;
      with theopts[1] do 
      begin
         name := 'help';
         has_arg := 0;
         flag := NIL;
         value := #0;
      end;
      with theopts[2] do 
      begin
         name := 'verbose';
         has_arg := 0;
         flag := NIL;
         value := #0;
      end;
      c := #0;
      repeat
         c := getlongopts('hv', @theopts[1], optionindex);
         case c of
            #0: 
            begin
               if(theopts[optionindex].name = 'help') then 
                  print_usage()
               else if(theopts[optionindex].name = 'verbose') then
                  verbose := 1;
            end; 
            'h': print_usage();
            'v': verbose := 1;
            '?', ':': 
            begin
               writeln();
               writeln('   (ERROR) Opcion incorrecta.');
               print_usage();
            end;
         end; { case }
      until(c = endofoptions);
      if(optind <= paramcount) then
      begin
         if(optind <= paramcount) then
         begin
            source_file := paramstr(optind);
            if not(existe(source_file)) then
            begin
               writeln();
               writeln('   (ERROR) Ruta a fichero de codigo fuente no valida.');
               print_usage();
            end;
            inc(optind);
         end;
         if(optind <= paramcount) then
         begin
            stat := true;
            stat_file := paramstr(optind);
            inc(optind);
         end;
         if(optind <= paramcount) then
            print_usage();
      end;
   end;

   { Muestra informacion de uso del programa }
   procedure print_usage();
   begin
      writeln();
      writeln(' ***** Analizador sintactico descendente recursivo',
              ' predictivo para Pascal- *****');
      writeln();
      writeln('   Modo de uso:');
      writeln('      ./p', ' <Fichero_fuente>');
      writeln('      ./p', ' <Fichero_fuente> <Fichero_estadisticas>');
      writeln('      ./p', ' [-v | --verbose] <Fichero_fuente>');
      writeln('      ./p', ' [-h | --help]');
      writeln();
      { writeln('   Los argumentos obligatorios para las opciones',
              ' largas son tambien obligatorios para las opciones',
              ' cortas.'); }
      writeln('   Opciones:');
      writeln('      -v, --verbose       Muestra informacion detallada',
              ' en la ejecucion del programa.');
      writeln('      -h, --help          Muestra informacion de ayuda.');
      writeln();
      writeln(' ****************************************************',
              '***************************');
      writeln();
      writeln('   { Practica realizada por Luis Carlos Garcia-Peraza',
              ' Herrera. }');
      writeln('   { Asignatura de Compiladores, curso 08/09. }');
      writeln();
      writeln(' ****************************************************',
              '***************************');
      writeln();
      halt();
   end;

begin

end.
