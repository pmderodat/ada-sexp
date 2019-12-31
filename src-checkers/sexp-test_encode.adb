--  Test program. Read a JSON description of an S-Expression on the standard
--  input and emit the corresponding S-Expression on the standard output.

with Ada.Text_IO;

with GNATCOLL.JSON;

with Sexp;
with Sexp.Generic_Dump;

procedure Sexp.Test_Encode is

   package IO renames Ada.Text_IO;
   package J renames GNATCOLL.JSON;

   type Stdout_Stream is null record;

   procedure Put (Stream : in out Stdout_Stream; Bytes : String);
   --  Callback for Sexp.Generic_Dump

   function From_JSON (Value : J.JSON_Value) return Sexp.Sexp_Value;
   --  Interpret the given JSON description (Value) and return the
   --  corresponding S-Expression.

   ---------
   -- Put --
   ---------

   procedure Put (Stream : in out Stdout_Stream; Bytes : String) is
      pragma Unreferenced (Stream);
   begin
      IO.Put (Bytes);
   end Put;

   ---------------
   -- From_JSON --
   ---------------

   function From_JSON (Value : J.JSON_Value) return Sexp.Sexp_Value is
      Kind : constant String := Value.Get ("kind");
   begin
      if Kind = "nil" then
         return Nil_Value;
      elsif Kind = "symbol" then
         return Create_Symbol (Value.Get ("value"));
      elsif Kind = "integer" then
         return Create_Integer (Value.Get ("value"));
      elsif Kind = "string" then
         return Create_String (Value.Get ("value"));
      elsif Kind = "cons" then
         return Create_Cons (From_JSON (Value.Get ("car")),
                             From_JSON (Value.Get ("cdr")));
      elsif Kind = "boolean" then
         return Create_Boolean (Value.Get ("value"));
      else
         raise Constraint_Error;
      end if;
   end From_JSON;

   procedure Dump is new Sexp.Generic_Dump (Stdout_Stream, Put);

   Input       : US.Unbounded_String;
   Description : J.JSON_Value;
   Result      : Sexp.Sexp_Value;
   Stdout      : Stdout_Stream := (null record);
begin
   --  Read the stdin until end of file and store its content in Input

   loop
      begin
         declare
            Line : constant String := IO.Get_Line;
         begin
            US.Append (Input, Line);
         end;
      exception
         when IO.End_Error =>
            exit;
      end;
   end loop;

   --  Decode this input as JSON

   Description := J.Read (US.To_String (Input));

   --  Build a S-Expression from the JSON description and output it on the
   --  standard output.

   Result := From_JSON (Description);
   Dump (Stdout, Result);
end Sexp.Test_Encode;
