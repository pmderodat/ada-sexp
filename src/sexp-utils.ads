--  Various helpers for Sexp

private package Sexp.Utils with Preelaborate is

   function Stripped_Image (I : Integer) return String;
   --  Return I'Image without the annoying leading character for positive
   --  values.

end Sexp.Utils;
