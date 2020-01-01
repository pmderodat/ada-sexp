--  Data structures to represent S-Expressions.
--
--  This package maps to a simple model: Sexp_Value represents a single
--  S-Expression value (atom or cons). Sexp_Value_Kind allows to discriminate
--  Sexp_Value objects depending on the kind of data they embed (string,
--  integer, cons, ...). Each value kind gets its constructor and getter(s).

private with Ada.Finalization;
with Ada.Strings.Unbounded;

package Sexp with Preelaborate is

   package US renames Ada.Strings.Unbounded;

   type Sexp_Value_Kind is
     (Nil, Symbol, Integer_Kind, String_Kind, Cons, Boolean_Kind);
   --  Kind of data that Sexp_Value objects wrap

   type Sexp_Value is tagged private;
   --  Single S-Expression value

   function Kind (Value : Sexp_Value) return Sexp_Value_Kind;
   --  Kind for the given S-Expression Value

   Nil_Value : constant Sexp_Value;
   --  Singleton for the nil S-Expression

   --  The following Sexp_Value subtypes allow concise contracts in subprogram
   --  signatures.

   subtype Symbol_Value is Sexp_Value
      with Dynamic_Predicate => Symbol_Value.Kind = Symbol;
   subtype Integer_Value is Sexp_Value
      with Dynamic_Predicate => Integer_Value.Kind = Integer_Kind;
   subtype String_Value is Sexp_Value
      with Dynamic_Predicate => String_Value.Kind = String_Kind;
   subtype Cons_Value is Sexp_Value
      with Dynamic_Predicate => Cons_Value.Kind = Cons;
   subtype Boolean_Value is Sexp_Value
      with Dynamic_Predicate => Boolean_Value.Kind = Boolean_Kind;

   ---------------------
   -- Symbol handling --
   ---------------------

   function Is_Valid_Symbol_Name (Name : String) return Boolean;
   function Create_Symbol (Name : String) return Sexp_Value
      with Pre  => Is_Valid_Symbol_Name (Name),
           Post => Create_Symbol'Result.Kind = Symbol;
   function As_Symbol_String (Value : Symbol_Value'Class) return String
      with Post => Is_Valid_Symbol_Name (As_Symbol_String'Result);

   ----------------------
   -- Integer handling --
   ----------------------

   function Create_Integer (Value : Integer) return Sexp_Value
      with Post => Create_Integer'Result.Kind = Integer_Kind;
   function As_Integer (Value : Integer_Value'Class) return Integer;

   ---------------------
   -- String handling --
   ---------------------

   function Create_String (Value : String) return Sexp_Value
      with Post => Create_String'Result.Kind = String_Kind;
   function As_String (Value : String_Value'Class) return String;

   -------------------
   -- Cons handling --
   -------------------

   function Create_Cons (Car, Cdr : Sexp_Value) return Sexp_Value
      with Post => Create_Cons'Result.Kind = Cons;
   function Car (Cons : Cons_Value'Class) return Sexp_Value;
   function Cdr (Cons : Cons_Value'Class) return Sexp_Value;

   ----------------------
   -- Boolean handling --
   ----------------------

   function Create_Boolean (Value : Boolean) return Sexp_Value
      with Post => Create_Boolean'Result.Kind = Boolean_Kind;
   function As_Boolean (Value : Boolean_Value'Class) return Boolean;

   ------------------
   -- Input/Output --
   ------------------

   type Source_Location is record
      Line, Column : Natural;
   end record;

   No_Location : constant Source_Location := (0, 0);

   --  Result of S-Expression parsing. If parsing was successful, contains the
   --  corresponding S-Expression value. Otherwise, contains an error message
   --  that describes why parsing failed.

   type Read_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Message  : US.Unbounded_String;
            Location : Source_Location;
         when True =>
            Value : Sexp_Value;
      end case;
   end record;

   function Parse_String (Buffer : String) return Read_Result;
   --  Parse the S-Expression in Buffer

   function Serialize_As_String (Value : Sexp.Sexp_Value) return String;
   --  Serialize the Value S-Expression as a string

   function Format_Error (Result : Read_Result) return String
      with Pre => not Result.Success;
   --  Format the error information in Result into a GNU-style diagnostic

private

   type Sexp_Access is access Sexp_Value;
   type Sexp_Data (Kind : Sexp_Value_Kind := Nil) is record
      case Kind is
         when Nil          => null;
         when Symbol       => Symbol_Value  : US.Unbounded_String;
         when Integer_Kind => Integer_Value : Integer;
         when String_Kind  => String_Value  : US.Unbounded_String;
         when Cons         => Car, Cdr      : Sexp_Access;
         when Boolean_Kind => Boolean_Value : Boolean;
      end case;
   end record;

   type Sexp_Value is new Ada.Finalization.Controlled with record
      Ref_Count : Natural;
      Data      : Sexp_Data;
   end record;

   overriding procedure Initialize (Value : in out Sexp_Value);
   overriding procedure Adjust (Value : in out Sexp_Value);
   overriding procedure Finalize (Value : in out Sexp_Value);

   Nil_Value : constant Sexp_Value := (Ada.Finalization.Controlled with
                                       Ref_Count => 0,
                                       Data      => (Kind => Nil));

end Sexp;
