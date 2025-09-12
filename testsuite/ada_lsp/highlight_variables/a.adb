
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body A is

   C : Boolean := True;

   type SemanticTokenModifiers is
     (declaration,
      definition,
      readonly,
      static,
      deprecated,
      an_abstract,
      async,
      modification,
      documentation,
      defaultLibrary,
      globalVariable,
      localVariable);

   type Modifiers_Array is array (SemanticTokenModifiers) of Boolean;

   -----------
   -- Print --
   -----------

   procedure Print is
      B : Boolean := True;

      function Correct_Name (Value : String) return String;

      function Correct_Name (Value : String) return String is
      begin
         return Value;
      end Correct_Name;

      function Get_Modifiers (Modifiers : Modifiers_Array) return String
      is
         use Ada.Strings.Unbounded;

         M : Unbounded_String;
      begin
         for Index in SemanticTokenModifiers'Range loop
            if Modifiers (Index) then
               if Index = globalVariable
                 or else Index = localVariable
               then
                  M := "-" & Correct_Name
                    (SemanticTokenModifiers'Image (Index)) & M;
               else
                  Append (M, "-" & Correct_Name
                          (SemanticTokenModifiers'Image (Index)));
               end if;
            end if;
         end loop;

         return To_String (M);
      end Get_Modifiers;

   begin
      Ada.Text_IO.Put_Line ("A");
      if B
        and then C
        and then D
      then
         null;
      end if;

      Ada.Text_IO.Put_Line
        (Get_Modifiers ((deprecated => True, others => False)));
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : String) is
   begin
      Ada.Text_IO.Put_Line (Value);
   end Print;

end A;
