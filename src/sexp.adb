with Ada.Unchecked_Deallocation;

with Sexp.Generic_Dump;
with Sexp.Generic_Parse;
with Sexp.Utils;

package body Sexp is

   function Allocate (Value : Sexp_Value) return Sexp_Access;
   procedure Inc_Ref (Value : Sexp_Access);
   procedure Dec_Ref (Value : in out Sexp_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Sexp_Value, Sexp_Access);

   procedure Dump_To_String is new Sexp.Generic_Dump
     (Output_Stream => US.Unbounded_String,
      Put           => US.Append);

   --------------
   -- Allocate --
   --------------

   function Allocate (Value : Sexp_Value) return Sexp_Access is
   begin
      return Result : constant Sexp_Access := new Sexp_Value'(Value) do
         Result.Ref_Count := Result.Ref_Count + 1;
      end return;
   end Allocate;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Value : Sexp_Access) is
   begin
      Value.Ref_Count := Value.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Value : in out Sexp_Access) is
   begin
      if Value /= null then
         Value.Ref_Count := Value.Ref_Count - 1;
         if Value.Ref_Count = 0 then
            Free (Value);
         end if;
      end if;
   end Dec_Ref;

   ----------
   -- Kind --
   ----------

   function Kind (Value : Sexp_Value) return Sexp_Value_Kind is
   begin
      return Value.Data.Kind;
   end Kind;

   --------------------------
   -- Is_Valid_Symbol_Name --
   --------------------------

   function Is_Valid_Symbol_Name (Name : String) return Boolean is
      pragma Unreferenced (Name);
   begin
      --  TODO
      return True;
   end Is_Valid_Symbol_Name;

   -------------------
   -- Create_Symbol --
   -------------------

   function Create_Symbol (Name : String) return Sexp_Value is
   begin
      return (Ada.Finalization.Controlled with
              Ref_Count => 0,
              Data      => (Kind         => Symbol,
                            Symbol_Value => US.To_Unbounded_String (Name)));
   end Create_Symbol;

   ----------------------
   -- As_Symbol_String --
   ----------------------

   function As_Symbol_String (Value : Symbol_Value'Class) return String is
   begin
      return US.To_String (Value.Data.Symbol_Value);
   end As_Symbol_String;

   --------------------
   -- Create_Integer --
   --------------------

   function Create_Integer (Value : Integer) return Sexp_Value is
   begin
      return (Ada.Finalization.Controlled with
              Ref_Count => 0,
              Data      => (Kind          => Integer_Kind,
                            Integer_Value => Value));
   end Create_Integer;

   function As_Integer (Value : Integer_Value'Class) return Integer is
   begin
      return Value.Data.Integer_Value;
   end As_Integer;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Value : String) return Sexp_Value is
   begin
      return (Ada.Finalization.Controlled with
              Ref_Count => 0,
              Data      => (Kind         => String_Kind,
                            String_Value => US.To_Unbounded_String (Value)));
   end Create_String;

   ---------------
   -- As_String --
   ---------------

   function As_String (Value : String_Value'Class) return String is
   begin
      return US.To_String (Value.Data.String_Value);
   end As_String;

   -----------------
   -- Create_Cons --
   -----------------

   function Create_Cons (Car, Cdr : Sexp_Value) return Sexp_Value is
   begin
      return (Ada.Finalization.Controlled with
              Ref_Count => 0,
              Data      => (Kind => Cons,
                            Car  => Allocate (Car),
                            Cdr  => Allocate (Cdr)));
   end Create_Cons;

   ---------
   -- Car --
   ---------

   function Car (Cons : Cons_Value'Class) return Sexp_Value is
   begin
      return Cons.Data.Car.all;
   end Car;

   ---------
   -- Cdr --
   ---------

   function Cdr (Cons : Cons_Value'Class) return Sexp_Value is
   begin
      return Cons.Data.Cdr.all;
   end Cdr;

   --------------------
   -- Create_Boolean --
   --------------------

   function Create_Boolean (Value : Boolean) return Sexp_Value is
   begin
      return (Ada.Finalization.Controlled with
              Ref_Count => 0,
              Data      => (Kind          => Boolean_Kind,
                            Boolean_Value => Value));
   end Create_Boolean;

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Value : Boolean_Value'Class) return Boolean is
   begin
      return Value.Data.Boolean_Value;
   end As_Boolean;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String (Buffer : String) return Read_Result is
      type Input_Stream is record
         Next_Character : Positive;
         --  Index of the next character in Buffer that Get must return
      end record;

      procedure Get
        (Stream : in out Input_Stream;
         EOF    : out Boolean;
         Byte   : out Character);
      --  Callback for Generic_Parse

      ---------
      -- Get --
      ---------

      procedure Get
        (Stream : in out Input_Stream;
         EOF    : out Boolean;
         Byte   : out Character)
      is
      begin
         if Stream.Next_Character > Buffer'Length then
            EOF := True;
         else
            EOF := False;
            Byte := Buffer (Stream.Next_Character);
            Stream.Next_Character := Stream.Next_Character + 1;
         end if;
      end Get;

      function Parse is new Sexp.Generic_Parse (Input_Stream, Get);

      Stream : Input_Stream := (Next_Character => Buffer'First);
   begin
      return Parse (Stream);
   end Parse_String;

   -------------------------
   -- Serialize_As_String --
   -------------------------

   function Serialize_As_String (Value : Sexp.Sexp_Value) return String is
      Result : US.Unbounded_String;
   begin
      Dump_To_String (Result, Value);
      return US.To_String (Result);
   end Serialize_As_String;

   ------------------
   -- Format_Error --
   ------------------

   function Format_Error (Result : Read_Result) return String is
      use US;
      Formatted : Unbounded_String;
   begin
      if Result.Location.Line /= 0 then
         Append (Formatted, Utils.Stripped_Image (Result.Location.Line));
         Append (Formatted, ':');
         if Result.Location.Column /= 0 then
            Append (Formatted, Utils.Stripped_Image (Result.Location.Column));
            Append (Formatted, ':');
         end if;
         Append (Formatted, ' ');
      end if;
      Append (Formatted, Result.Message);
      return To_String (Formatted);
   end Format_Error;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Value : in out Sexp_Value) is
   begin
      Value := Nil_Value;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Value : in out Sexp_Value) is
      D : Sexp_Data renames Value.Data;
   begin
      if D.Kind = Cons then
         Inc_Ref (D.Car);
         Inc_Ref (D.Cdr);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Value : in out Sexp_Value) is
      D : Sexp_Data renames Value.Data;
   begin
      if D.Kind = Cons then
         Dec_Ref (D.Car);
         Dec_Ref (D.Cdr);
      end if;
   end Finalize;

end Sexp;
