--  Test program. Read bytes on the standard input as a S-Expression.
--
--  If it's a valid one, parse it and emit on the standard output a JSON
--  representation of it. If it's not a valid one, print an error message on
--  the standard output.

with Ada.Command_Line;
with Ada.Text_IO;

with GNATCOLL.JSON;

with Sexp.Generic_Parse;

procedure Sexp.Test_Decode is

   package Cmd renames Ada.Command_Line;
   package IO renames Ada.Text_IO;
   package J renames GNATCOLL.JSON;

   type Stdin_Stream is null record;

   procedure Get
     (Stream : in out Stdin_Stream; EOF : out Boolean; Byte : out Character);
   --  Callback for Sexp.Generic_Parse

   function To_JSON (Value : Sexp.Sexp_Value) return J.JSON_Value;
   --  Turn the given S-Expression value into the corresponding JSON
   --  representation.

   -------------
   -- To_JSON --
   -------------

   function To_JSON (Value : Sexp.Sexp_Value) return J.JSON_Value is
      Inner  : J.JSON_Value;
   begin
      return Result : constant J.JSON_Value := J.Create_Object do
         Result.Set_Field
           ("kind",
            (case Value.Kind is
             when Nil          => "nil",
             when Symbol       => "symbol",
             when Integer_Kind => "integer",
             when String_Kind  => "string",
             when Cons         => "cons",
             when Boolean_Kind => "boolean"));
         case Value.Kind is
            when Nil =>
               return;
            when Symbol =>
               Inner := J.Create (Value.As_Symbol_String);
            when Integer_Kind =>
               Inner := J.Create (Value.As_Integer);
            when String_Kind =>
               Inner := J.Create (Value.As_String);
            when Cons =>
               Result.Set_Field ("car", To_JSON (Value.Car));
               Result.Set_Field ("cdr", To_JSON (Value.Cdr));
               return;
            when Boolean_Kind =>
               Inner := J.Create (Value.As_Boolean);
         end case;
         Result.Set_Field ("value", Inner);
      end return;
   end To_JSON;

   ---------
   -- Get --
   ---------

   procedure Get
     (Stream : in out Stdin_Stream; EOF : out Boolean; Byte : out Character)
   is
      pragma Unreferenced (Stream);
      Available : Boolean;
   begin
      IO.Get_Immediate (Byte, Available);
      EOF := not Available;
   exception
      when IO.End_Error =>
         EOF := True;
   end Get;

   function Parse is new Sexp.Generic_Parse (Stdin_Stream, Get);

   Stdin  : Stdin_Stream := (null record);
   Result : constant Sexp.Read_Result := Parse (Stdin);
begin
   if Result.Success then
      IO.Put_Line (To_JSON (Result.Value).Write);
   else
      IO.Put_Line (Sexp.Format_Error (Result));
      Cmd.Set_Exit_Status (Cmd.Failure);
   end if;
end Sexp.Test_Decode;
