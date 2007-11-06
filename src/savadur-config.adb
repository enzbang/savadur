
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Sax.Readers;
with Sax.Attributes;

with Input_Sources.File;
with Unicode.CES;

package body Savadur.Config is

   use Ada;
   use Ada.Strings.Unbounded;

   --  SAX overloaded routines to parse the incoming XML stream.

   type Tree_Reader is new Sax.Readers.Reader with record
      Key   : Unbounded_String;
      Value : Unbounded_String;
   end record;

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "");

   procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence);

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence) is
   begin
      Append (Handler.Value, To_Unbounded_String (Ch));
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Local_Name);
      pragma Unreferenced (Qname);

   begin
      if Handler.Key /= Null_Unbounded_String then
         Text_IO.Put_Line (To_String (Handler.Key));
         Text_IO.Put_Line (To_String (Handler.Value));
      end if;

      Handler.Key   := Null_Unbounded_String;
      Handler.Value := Null_Unbounded_String;
   exception
      when others =>
         null;
   end End_Element;

   -----------
   -- Parse --
   -----------

   procedure Parse (Filename : String) is
      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;
   begin
      Input_Sources.File.Open
        (Filename => Filename,
         Input    => Source);

      Parse (Reader, Source);

      Input_Sources.File.Close (Source);

   end Parse;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Qname);

      use Sax.Attributes;

   begin
      Handler.Key := To_Unbounded_String (Local_Name);

      --  Read all attributes, add a key/value pair for each atributes into
      --  the table with [Local_Name & '.'] added in from of the key (attribute
      --  name)

      for J in 0 .. Get_Length (Atts) - 1 loop
         declare
            Key : constant String := Local_Name & '.' & Get_Qname (Atts, J);
         begin
            Text_IO.Put_Line (Key);
            Text_IO.Put_Line (Get_Value (Atts, J));
         end;
      end loop;
   end Start_Element;

end Savadur.Config;
