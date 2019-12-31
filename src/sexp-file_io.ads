with Ada.Text_IO;

package Sexp.File_IO is

   --  Subprograms to load/save S-Expressions from/to files

   use all type Ada.Text_IO.File_Mode;

   function Load_File (Filename : String) return Read_Result;
   --  Read Filename and parse its content as an S-Expression

   function Load_File (File : in out Ada.Text_IO.File_Type) return Read_Result
      with Pre => Ada.Text_IO.Mode (File) = In_File;
   --  Read Filename and parse its content as an S-Expression

   procedure Dump_To_File
     (Value : Sexp_Value; File : in out Ada.Text_IO.File_Type)
      with Pre => Ada.Text_IO.Mode (File) in Out_File | Append_File;
   --  Serialize the Value S-Expression and write it to File

end Sexp.File_IO;
