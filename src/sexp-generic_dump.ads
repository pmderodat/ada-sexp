generic
   type Output_Stream (<>) is limited private;
   --  Stream of bytes

   with procedure Put (Stream : in out Output_Stream; Bytes : String) is <>;
   --  Write all Bytes in Stream

procedure Sexp.Generic_Dump
  (Stream : in out Output_Stream;
   Value  : Sexp_Value)
   with Preelaborate;
--  Turn the given S-Expression Value into a string and write it to Stream
