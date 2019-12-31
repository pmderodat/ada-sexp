package body Sexp.Utils is

   --------------------
   -- Stripped_Image --
   --------------------

   function Stripped_Image (I : Integer) return String is
      Result : constant String := I'Image;
   begin
      return Result ((if I < 0 then Result'First else Result'First + 1)
                     .. Result'Last);
   end Stripped_Image;

end Sexp.Utils;
