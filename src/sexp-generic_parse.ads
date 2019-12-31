generic
   type Input_Stream (<>) is limited private;
   --  Stream of bytes

   with procedure Get (Stream : in out Input_Stream;
                       EOF    : out Boolean;
                       Byte   : out Character) is <>;
   --  Try to read a byte from Stream. If the end of Stream was reached before
   --  we could read such a byte, just set EOF to True. Otherwise, set it to
   --  False and put the read character to Byte.

   Tab_Stop : Positive := 8;
   --  Maximal number of columns that tab characters (0x09) skip

function Sexp.Generic_Parse
  (Stream : in out Input_Stream) return Sexp.Read_Result
  with Preelaborate;
--  Read a S-Expression from Stream and return the corresponding value
