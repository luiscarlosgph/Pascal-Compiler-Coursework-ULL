program prueba;
const
   A = A;     { Error }
   B = false;
   C = 1234;
   D = C;
   E = 10;
type
   t = array[1..10] of integer;
   t2 = array[1..22] of t;
   v1 = array[10..10] of boolean;
   v2 = array[22..10] of integer; { Error }
   v3 = array[10..20] of C; { Error }
   v4 = array[1..2] of v4; { Error }
   Tjuas = record
              a, b, c : t;
              t : t;
              t2 : t2;
              false : boolean;
              integer : integer
           end;
   Tmalo = record
              C : integer;
              C : boolean; { Error }
              dudu, h, i, j : integer;
              jaja, l, m : boolean;
              false : boolean
	   end;
   TipoA = record
              jiji : integer;
              hola : molestar; { Error }
              campo : TipoA; { Error }
              TipoA : boolean
           end;
   v5 = array[0..222] of Tjuas; 
   v6 = array[2..22] of v5;
var 
   jaja : integer;
   x : integer;
   y : boolean;
   si, no, juas, ole : boolean;
   si, no : integer; { Error }
   { Cuidado con esto ya que como en la 
     tabla de simbolos los ultimos 'si'
     y 'no' que insertamos son integer
     al buscar los encontraremos primero }
   juas, ole : boolean; { Error }
   molestar : integer;
   prueba : boolean; { Error }
   ac1 : Tjuas;
   ac2 : Tmalo;
   ac3 : TipoA;
   TipoA : TipoA; { Error }
   uf : v6;
   erroneo : rana; { Error }
   
   { Funciones y procedimientos }
   procedure inutil;
   const 
      A = x;  { Error }
      A = 0106; { Error }
   var
      B : boolean;
      C, D : integer;
   begin
   end;

   procedure masinutil(var a, b : boolean;
                       c, d : integer;
                       ultimo : boolean);
   var
      a : boolean; { Error }
      otro, mas : integer;
   begin

   end;

begin
   { Acceso a variables }
   C := 1; { Error }
   x; { Error }
   x := 2210; 
   x := false; { Error }
   si := true; { Error }
   prueba := false;
   si := 1;
   jaja := A; 
   jaja := x;
   ac1.a[3] := 22;
   ac1.a[true] := 10; { Error }
   ac1.t[2] := + 2 - 1234;
   ac1.t[2][1] := 3; { Error }
   ac1.nofield := ((())); { Error }
   ac1.ac1 := 0; { Error }
   ac1.integer := 5;
   ac1.false := true;
   ac2.C := false;
   ac2.dudu := 9;
   ac2.h := 8; 
   ac2.i := 7; 
   ac2.false := true;
   ac3.jiji := ac1.false; { Error } 
   ac3.hola := 2; 
   ac3.campo := ac2.C; 
   ac3.TipoA := false;
   x := ac2.i;
   ac2.j := 6;
   jaja := ac2.j;
   ac1.t2[ac1.a[3]][ac2.h] := (ac1.t[2] + x * (2 + 7 div (4 * 3)));
   ac1.t2[ac1.t[2]][ac1.integer] := (ac1.t[2] + ac1.t2[2][ac1.integer]);
   uf[4][2].integer := 1;
   uf[E][2].integer := 10;
   uf[2][ac2.i].false := false;
   C.a := 10; { Error } 
   { Procedimientos }
   write(7);
   write(ac1.t[2] + x * (2 + 7 div (4 * 3)));
   write; { Error }
   write(); { Error }
   write(true); { Error }
   write(1, 2); { Error }
   read(x);
   read(ac1.integer);
   read(22); { Error }
   read(x, x); { Error }
   read(ac2.false); { Error }
   read; { Error }
   read(); { Error }
   inutil;
   inutil(); { Error }
   masinutil(); { Error }
   masinutil; { Error }
   masinutil(ac3.TipoA, ac2.C, 22, 10, false); 
   masinutil(y, y, ac2.j, (1 + C div ac1.t[2]), false);
   masinutil(false, true); { Error }
   masinutil(juas, ole, ac1.t2[ac1.a[3]][ac2.h], ac1.t2[ac1.a[3]][ac2.h], ac2.C, true); { Error } 
   masinutil(ac1.t2[ac1.a[3]][ac2.h], ac1.t2[ac1.a[3]][ac2.h], ac2.C, true); { Error } 
   masinutil(1, 2, 3, 4, 5); { Error }
   masinutil(C, true, ac2.j, (1 + C div ac1.t[2]), false); { Error }
   masinutil(ac3.TipoA, ac3.TipoA, false, ac2.dudu, true); { Error }
   masinutil(true, false, 22, 22, ac2.C); { Error - valor / variable } 
   erroneo; { Error }
   rana; { Error }
   { Expresiones }
   x := 3 + 4 * (x div true * false); { Error }
   x := true mod 3; { Error }
   x := x mod (ac2.j * (C + false)); { Error }
   x := C * true; { Error }
   x := false * ac2.j; { Error }
   x := true div C; { Error }
   x := 2210 mod false; { Error }   
   x := 2 + 1 * (x + (true and false)); { Error }
   x := 22 and 10; { Error }
   x := ac1.a[3] div ac1.a[3] + x;
   ac2.false := false and (true or false) or false and (ac1.false or true); 
   ac1.a[3] := (((((x)))));
   ac1.a[3] := (); { Error } 
   ac1.a[3] + x := 22; { Error }
   { ... }
   x := ac1.a[3] div (- 22 + 22); { Error' }
   ac1.a[9999] := 0; { Error' }   
end.
