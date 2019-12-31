with Sexp.Utils;

procedure Sexp.Generic_Dump
  (Stream : in out Output_Stream;
   Value  : Sexp_Value)
is
begin
   case Value.Kind is
      when Nil =>
         Put (Stream, "()");

      when Symbol =>
         Put (Stream, Value.As_Symbol_String);

      when Integer_Kind =>
         Put (Stream, Sexp.Utils.Stripped_Image (Value.As_Integer));

      when String_Kind =>
         Put (Stream, """");
         for C of Value.As_String loop
            case C is
               when ASCII.LF =>
                  Put (Stream, "\n");
               when ASCII.HT =>
                  Put (Stream, "\t");
               when '"' =>
                  Put (Stream, "\""");
               when '\' =>
                  Put (Stream, "\\");
               when others =>
                  Put (Stream, (1 => C));
            end case;
         end loop;
         Put (Stream, """");

      when Cons =>
         declare
            V : Sexp_Value := Value;
         begin
            Put (Stream, "(");
            loop
               Generic_Dump (Stream, V.Car);
               V := V.Cdr;
               case V.Kind is
                  when Nil =>
                     exit;

                  when Cons =>
                     Put (Stream, " ");

                  when others =>
                     Put (Stream, " . ");
                     Generic_Dump (Stream, V);
                     exit;
               end case;
            end loop;
            Put (Stream, ")");
         end;

      when Boolean_Kind =>
         Put (Stream, (if Value.As_Boolean then "#t" else "#f"));
   end case;
end Sexp.Generic_Dump;
