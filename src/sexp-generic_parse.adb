with Ada.Containers.Vectors;

function Sexp.Generic_Parse
  (Stream : in out Input_Stream) return Sexp.Read_Result
is
   Result : Read_Result := (Success => True, Value => <>);

   function Set_Error
     (Location : Source_Location; Message : String) return Boolean;

   -----------------------
   -- Character reading --
   -----------------------

   type Char_Read (EOF : Boolean := False) is record
      Location  : Source_Location;
      To_Reemit : Boolean;
      case EOF is
         when False => Value : Character;
         when True  => null;
      end case;
   end record;

   Horizontal_Tab : constant Character := ASCII.HT;
   Cariage_Return : constant Character := ASCII.CR;
   Line_Feed      : constant Character := ASCII.LF;

   subtype Any_Whitespace is Character with Static_Predicate =>
      Any_Whitespace in ' ' | Horizontal_Tab | Cariage_Return | Line_Feed;
   subtype Any_Digit is Character range '0' .. '9';
   subtype Any_Delimiter is Character with Static_Predicate =>
      Any_Delimiter in '"' | '(' | ')' | ';';

   Char : Char_Read :=
     (EOF => False, Location => (1, 0), To_Reemit => False, Value => <>);

   procedure Read_Char with Pre => not Char.EOF;
   procedure Reemit_Char with Pre => not Char.To_Reemit;
   function Set_Unexpected_EOF_Error return Boolean is
     (Set_Error (Char.Location, "unexpected end of file"));
   function Has_Token_Separator return Boolean is
     (Char.EOF or else Char.Value in Any_Whitespace | Any_Delimiter);

   -------------------
   -- Token reading --
   -------------------

   type Any_Token_Kind is
     (EOF, Open_Paren, Close_Paren, Dot_Token, True_Token, False_Token,
      Symbol_Token, String_Token, Integer_Token);
   subtype Sign_Token_Kind is
      Any_Token_Kind range Open_Paren .. False_Token;
   subtype Text_Token_Kind is
      Any_Token_Kind range Symbol_Token ..  Integer_Token;

   type Token_Read (Kind : Any_Token_Kind := Close_Paren) is record
      Location  : Source_Location;
      To_Reemit : Boolean;
      case Kind is
         when EOF | Sign_Token_Kind => null;
         when Text_Token_Kind => Text : US.Unbounded_String;
      end case;
   end record;

   Token : Token_Read := (Kind      => Close_Paren,
                          Location  => (0, 0),
                          To_Reemit => False,
                          Text      => <>);

   function Read_Token return Boolean with Pre => Token.Kind /= EOF;
   procedure Reemit_Token with Pre => not Token.To_Reemit;

   ---------------------
   -- Parsing helpers --
   ---------------------

   package Sexp_Value_Vectors is new Ada.Containers.Vectors
     (Positive, Sexp_Value);

   function Parse (Value : out Sexp_Value) return Boolean;

   ---------------
   -- Set_Error --
   ---------------

   function Set_Error
     (Location : Source_Location; Message : String) return Boolean
   is
   begin
      Result := (Success  => False,
                 Message  => US.To_Unbounded_String (Message),
                 Location => Location);
      return False;
   end Set_Error;

   ---------------
   -- Read_Char --
   ---------------

   procedure Read_Char is
      Previous_Location : constant Source_Location := Char.Location;
      Next_Location     : Source_Location :=
        (Previous_Location.Line, Previous_Location.Column + 1);
      Has_EOF           : Boolean;
      Byte              : Character;
   begin
      if Char.To_Reemit then
         Char.To_Reemit := False;
      else
         Get (Stream, Has_EOF, Byte);
         if Has_EOF then
            Char :=
              (EOF => True, Location => Next_Location, To_Reemit => False);
         else
            case Byte is
               when ASCII.LF =>
                  Next_Location := (Next_Location.Line + 1, 1);
               when ASCII.HT =>
                  Next_Location.Column :=
                     Tab_Stop * ((Next_Location.Column + Tab_Stop)
                                 mod Tab_Stop);
               when others =>
                  null;
            end case;
            Char := (EOF       => False,
                     Location  => Next_Location,
                     To_Reemit => False,
                     Value     => Byte);
         end if;
      end if;
   end Read_Char;

   -----------------
   -- Reemit_Char --
   -----------------

   procedure Reemit_Char is
   begin
      Char.To_Reemit := True;
   end Reemit_Char;

   ----------------
   -- Read_Token --
   ----------------

   function Read_Token return Boolean is
      Location : Source_Location;
      Text     : US.Unbounded_String;

      function Return_EOF return Boolean;
      function Return_Sign (Kind : Sign_Token_Kind) return Boolean;
      function Return_Text (Kind : Text_Token_Kind) return Boolean;

      ----------------
      -- Return_EOF --
      ----------------

      function Return_EOF return Boolean is
      begin
         Token := (Kind => EOF, Location => Location, To_Reemit => False);
         return True;
      end Return_EOF;

      -----------------
      -- Return_Sign --
      -----------------

      function Return_Sign (Kind : Sign_Token_Kind) return Boolean is
         Result : Token_Read (Kind);
      begin
         Result.Location := Location;
         Result.To_Reemit := False;
         Token := Result;
         return True;
      end Return_Sign;

      -----------------
      -- Return_Text --
      -----------------

      function Return_Text (Kind : Text_Token_Kind) return Boolean is
         Result : Token_Read (Kind);
      begin
         Result.Location := Location;
         Result.To_Reemit := False;
         Result.Text := Text;
         Token := Result;
         return True;
      end Return_Text;

   begin
      if Token.To_Reemit then
         Token.To_Reemit := False;
         return True;
      end if;

      --  Look for the beginning of a token

      Find_Token_Start : loop
         Read_Char;
         Location := Char.Location;

         if Char.EOF then
            return Return_EOF;

         elsif Char.Value in Any_Whitespace then
            null;

         elsif Char.Value = ';' then

            --  We found a comment: skip characters until the end of the line

            Skip_Line : loop
               Read_Char;
               if Char.EOF then
                  return Return_EOF;
               elsif Char.Value = ASCII.LF then
                  exit Skip_Line;
               end if;
            end loop Skip_Line;

         else
            --  We found something that is not EOF, not a whitespace and not a
            --  comment: this is the beginning of our token.

            exit Find_Token_Start;

         end if;
      end loop Find_Token_Start;

      case Char.Value is
         when '(' =>
            return Return_Sign (Open_Paren);
         when ')' =>
            return Return_Sign (Close_Paren);

         when '#' =>

            --  This is supposed to be a boolean (#t or #f). Once we got the
            --  expected bytes, make sure that the character that comes after
            --  it is a valid separator.

            declare
               Hash_Location : constant Source_Location := Char.Location;
               function Set_Error return Boolean is
                 (Set_Error (Hash_Location, "invalid hashed name"));
            begin
               Read_Char;
               if Char.EOF then
                  return Set_Unexpected_EOF_Error;
               elsif Char.Value in 't' | 'f' then
                  declare
                     Kind : constant Any_Token_Kind :=
                       (if Char.Value = 't' then True_Token else False_Token);
                  begin
                     Read_Char;
                     if Has_Token_Separator then
                        Reemit_Char;
                        return Return_Sign (Kind);
                     else
                        return Set_Error;
                     end if;
                  end;
               else
                  return Set_Error;
               end if;
            end;

         when '"' =>

            --  This character starts a string literal. Consume everything
            --  until the next double quote that is not escaped.

            loop
               Read_Char;
               if Char.EOF then
                  return Set_Unexpected_EOF_Error;
               end if;

               case Char.Value is
                  when '"' =>
                     return Return_Text (String_Token);

                  when '\' =>
                     --  This is an escape sequence: accept only:
                     --
                     --    \n \t \" and \\

                     declare
                        Backslash_Location : constant Source_Location :=
                           Char.Location;
                     begin
                        Read_Char;
                        if Char.EOF then
                           return Set_Unexpected_EOF_Error;
                        end if;
                        case Char.Value is
                           when 'n' => US.Append (Text, ASCII.LF);
                           when 't' => US.Append (Text, ASCII.HT);
                           when '"' => US.Append (Text, '"');
                           when '\' => US.Append (Text, '\');
                           when others =>
                              return Set_Error (Backslash_Location,
                                                "invalid escape sequence");
                        end case;
                     end;

                  when others =>
                     US.Append (Text, Char.Value);
               end case;
            end loop;

         when others =>
            --  Maybe we have an integer literal? We'll know for sure only
            --  after consuming it until the next token separator

            if Char.Value in '-' | Any_Digit then
               declare
                  Have_Digit : Boolean := Char.Value in Any_Digit;
               begin
                  loop
                     US.Append (Text, Char.Value);
                     Read_Char;

                     if Has_Token_Separator then

                        --  This new character closes this token. If we had at
                        --  least one digit, we know this is an integer
                        --  literal.  Otherwise, fallback to creating a symbol.

                        Reemit_Char;
                        return Return_Text
                          (if Have_Digit then Integer_Token else Symbol_Token);
                     end if;

                     --  If we have a non-digit, stop reading here and fallback
                     --  to creating a symbol.

                     if Char.Value not in Any_Digit then
                        Reemit_Char;
                        exit;
                     end if;

                     Have_Digit := True;
                  end loop;
               end;

            elsif Char.Value = '.' then

               --  We have a dot only if a separator comes after it. Otherwise,
               --  fallback to creating a symbol.

               US.Append (Text, Char.Value);
               Read_Char;
               if Has_Token_Separator then
                  Reemit_Char;
                  return Return_Sign (Dot_Token);
               end if;
               US.Append (Text, Char.Value);

            else
               US.Append (Text, Char.Value);

            end if;

            --  Consume all remaining characters until the next
            --  whitespace/separator and create a symbol token.

            loop
               Read_Char;
               if Has_Token_Separator then
                  Reemit_Char;
                  return Return_Text (Symbol_Token);
               end if;
               US.Append (Text, Char.Value);
            end loop;
      end case;
   end Read_Token;

   ------------------
   -- Reemit_Token --
   ------------------

   procedure Reemit_Token is
   begin
      Token.To_Reemit := True;
   end Reemit_Token;

   -----------
   -- Parse --
   -----------

   function Parse (Value : out Sexp_Value) return Boolean is
   begin
      if not Read_Token then
         return False;
      end if;

      case Token.Kind is
         when EOF =>
            return Set_Unexpected_EOF_Error;

         when True_Token =>
            Value := Create_Boolean (True);
            return True;
         when False_Token =>
            Value := Create_Boolean (False);
            return True;

         when Symbol_Token =>
            Value := Create_Symbol (US.To_String (Token.Text));
            return True;
         when String_Token =>
            Value := Create_String (US.To_String (Token.Text));
            return True;
         when Integer_Token =>
            Value := Create_Integer
              (Integer'Value (US.To_String (Token.Text)));
            return True;

         when Open_Paren =>
            --  If the next token is a closing paren, we have a nil value.
            --  Otherwise, this is a cons.

            if not Read_Token then
               return False;
            elsif Token.Kind = Close_Paren then
               Value := Nil_Value;
               return True;
            else
               Reemit_Token;
            end if;

            --  This is a cons: parse all S-Expressions inside it first. Once
            --  this is done, we will be able to build the tree of cons using
            --  the reverse parsing order.

            declare
               type Any_Dot_State is
                 (Not_Found, Just_Found, Before_Last_Value);
               Dot_State          : Any_Dot_State := Not_Found;
               First_Dot_Location : Source_Location;

               function Set_Dot_Error return Boolean is
                 (Set_Error (First_Dot_Location, "misplaced dot"));

               Values : Sexp_Value_Vectors.Vector;
               Next   : Sexp_Value;
            begin
               loop
                  --  Parse the next value

                  if not Parse (Next) then
                     return False;
                  end if;
                  Values.Append (Next);
                  case Dot_State is
                     when Not_Found =>
                        null;
                     when Just_Found =>
                        Dot_State := Before_Last_Value;
                     when Before_Last_Value =>
                        return Set_Dot_Error;
                  end case;

                  --  Check the next token...

                  if not Read_Token then
                     return False;

                  elsif Token.Kind = Close_Paren then

                     --  It's a closing paren: we now have all the values
                     --  needed to build the cons tree.

                     exit;

                  elsif Token.Kind = Dot_Token then

                     --  It's a dot token: take it into account. It is an error
                     --  to have one before the first value and we can have at
                     --  most one.

                     case Dot_State is
                        when Not_Found =>
                           Dot_State := Just_Found;
                        when others =>
                           return Set_Dot_Error;
                     end case;
                     First_Dot_Location := Token.Location;

                  else
                     --  This token is not related to the currently parsed tree
                     --  of cons: reemit it so that the next call to Parse uses
                     --  it.

                     Reemit_Token;
                  end if;
               end loop;

               --  We can finally build the tree of cons from Values

               case Dot_State is
                  when Not_Found =>
                     Value := Nil_Value;

                  when Just_Found =>

                     --  The only way to leave the loop above without returning
                     --  is to find a close paren token after successfully
                     --  parsing a token.

                     raise Program_Error;

                  when Before_Last_Value =>
                     declare
                        Car, Cdr : Sexp_Value;
                     begin
                        Cdr := Values.Last_Element;
                        Values.Delete_Last;
                        Car := Values.Last_Element;
                        Values.Delete_Last;
                        Value := Create_Cons (Car, Cdr);
                     end;
               end case;

               for Car of reverse Values loop
                  Value := Create_Cons (Car, Value);
               end loop;
               return True;
            end;

         when others =>
            return Set_Error (Token.Location, "unexpected token");
      end case;
   end Parse;

begin
   --  Try to parse a S-Expression. On success, make sure only EOF comes next.

   if Parse (Result.Value) then
      if Read_Token and then Token.Kind /= EOF then
         declare
            Dummy : constant Boolean :=
               Set_Error (Token.Location, "end of file expected");
         begin
            null;
         end;
      end if;
   end if;
   return Result;
end Sexp.Generic_Parse;
