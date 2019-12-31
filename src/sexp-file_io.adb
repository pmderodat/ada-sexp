with Ada.Exceptions;

with Sexp.Generic_Dump;
with Sexp.Generic_Parse;

package body Sexp.File_IO is

   procedure Get
     (Stream : in out Ada.Text_IO.File_Type;
      EOF    : out Boolean;
      Byte   : out Character);
   --  Callback for Parse_File

   function Parse_File is new Sexp.Generic_Parse
     (Input_Stream => Ada.Text_IO.File_Type,
      Get          => Get);

   procedure Put_To_File (File : in out Ada.Text_IO.File_Type; Bytes : String);
   --  Callback for Sexp.Generic_Dump

   procedure Dump_To_File is new Sexp.Generic_Dump
     (Output_Stream => Ada.Text_IO.File_Type,
      Put           => Put_To_File);

   ---------
   -- Get --
   ---------

   procedure Get
     (Stream : in out Ada.Text_IO.File_Type;
      EOF    : out Boolean;
      Byte   : out Character) is
   begin
      EOF := False;
      Ada.Text_IO.Get_Immediate (Stream, Byte);
   exception
      when Ada.Text_IO.End_Error =>
         EOF := True;
   end Get;

   -----------------
   -- Put_To_File --
   -----------------

   procedure Put_To_File (File : in out Ada.Text_IO.File_Type; Bytes : String)
   is
   begin
      Ada.Text_IO.Put (File, Bytes);
   end Put_To_File;

   ---------------
   -- Load_File --
   ---------------

   function Load_File (Filename : String) return Read_Result is
      use Ada.Exceptions, Ada.Text_IO;

      File : File_Type;
   begin
      begin
         Open (File, In_File, Filename);
      exception
         when Exc : Name_Error | Use_Error =>
            declare
               Message : constant String :=
                  "cannot open " & Filename & ": " & Exception_Message (Exc);
            begin
               return (Success  => False,
                       Message  => US.To_Unbounded_String (Message),
                       Location => (0, 0));
            end;
      end;

      return Result : constant Read_Result := Parse_File (File) do
         Close (File);
      end return;
   end Load_File;

   ---------------
   -- Load_File --
   ---------------

   function Load_File (File : in out Ada.Text_IO.File_Type) return Read_Result
   is
   begin
      return Parse_File (File);
   end Load_File;

   ------------------
   -- Dump_To_File --
   ------------------

   procedure Dump_To_File
     (Value : Sexp_Value; File : in out Ada.Text_IO.File_Type) is
   begin
      Dump_To_File (File, Value);
   end Dump_To_File;

end Sexp.File_IO;
