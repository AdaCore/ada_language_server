------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Utils;

with Laltools.Common;

with Libadalang.Analysis;        use Libadalang.Analysis;
with Libadalang.Common;          use Libadalang.Common;

with LSP.Ada_Completions.Filters;
with LSP.Ada_Completions.Generic_Assoc;
with LSP.Ada_Completions.Generic_Assoc_Utils;

with LSP.Lal_Utils;

with VSS.Strings.Conversions;

package body LSP.Ada_Completions.Parameters is

   function Get_Spec_Call_Expr_Designators
     (C       : Libadalang.Analysis.Call_Expr;
      Context : not null LSP.Ada_Handlers.Context_Access)
      return LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List;
   function Get_Prefix_Node
     (C      : Libadalang.Analysis.Call_Expr;
      Column : out Langkit_Support.Slocs.Column_Number)
      return Libadalang.Analysis.Ada_Node'Class;

   package Call_Expr_Completion is new
     LSP.Ada_Completions.Generic_Assoc
       (Element              => Libadalang.Analysis.Call_Expr,
        Null_Element         => Libadalang.Analysis.No_Call_Expr,
        Pretty_Print_Rule    => Libadalang.Common.Param_Assoc_Rule,
        Get_Prefix_Node      => Get_Prefix_Node,
        Search_Element       => LSP.Lal_Utils.Get_Call_Expr,
        Get_Designators      => LSP.Lal_Utils.Get_Call_Designators,
        Get_Spec_Designators => Get_Spec_Call_Expr_Designators);

   function Get_Aggregate
     (N : Libadalang.Analysis.Ada_Node'Class)
      return Libadalang.Analysis.Aggregate;
   function Get_Designators
     (A : Libadalang.Analysis.Aggregate)
      return Laltools.Common.Node_Vectors.Vector;
   function Get_Spec_Aggregate_Designators
     (A       : Libadalang.Analysis.Aggregate;
      Context : not null LSP.Ada_Handlers.Context_Access)
      return LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List;
   function Get_Prefix_Node
     (A      : Libadalang.Analysis.Aggregate;
      Column : out Langkit_Support.Slocs.Column_Number)
      return Libadalang.Analysis.Ada_Node'Class;

   package Aggregate_Completion is new
     LSP.Ada_Completions.Generic_Assoc
       (Element              => Libadalang.Analysis.Aggregate,
        Null_Element         => Libadalang.Analysis.No_Aggregate,
        Pretty_Print_Rule    => Libadalang.Common.Param_Assoc_Rule,
        Get_Prefix_Node      => Get_Prefix_Node,
        Search_Element       => Get_Aggregate,
        Get_Designators      => Get_Designators,
        Get_Spec_Designators => Get_Spec_Aggregate_Designators);

   function Get_Generic_Package
     (N : Libadalang.Analysis.Ada_Node'Class)
      return Libadalang.Analysis.Generic_Package_Instantiation;
   function Get_Designators
     (G : Libadalang.Analysis.Generic_Package_Instantiation)
      return Laltools.Common.Node_Vectors.Vector;
   function Get_Decl_Designators
     (G       : Libadalang.Analysis.Generic_Package_Instantiation;
      Context : not null LSP.Ada_Handlers.Context_Access)
      return LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List;
   function Get_Prefix_Node
     (G      : Libadalang.Analysis.Generic_Package_Instantiation;
      Column : out Langkit_Support.Slocs.Column_Number)
      return Libadalang.Analysis.Ada_Node'Class;

   package Generic_Package_Completion is new
     LSP.Ada_Completions.Generic_Assoc
       (Element              =>
           Libadalang.Analysis.Generic_Package_Instantiation,
        Null_Element         =>
           Libadalang.Analysis.No_Generic_Package_Instantiation,
        --  GNATpp is having trouble formatting a LAL tree representing
        --  Generic_Package_Instanciation_Rule
        --  => retrieve the nested Assoc_List and format it as a function
        --  call.
        Pretty_Print_Rule    => Libadalang.Common.Param_Assoc_Rule,
        Get_Prefix_Node      => Get_Prefix_Node,
        Search_Element       => Get_Generic_Package,
        Get_Designators      => Get_Designators,
        Get_Spec_Designators => Get_Decl_Designators);

   ------------------------------------
   -- Get_Spec_Call_Expr_Designators --
   ------------------------------------

   function Get_Spec_Call_Expr_Designators
     (C       : Libadalang.Analysis.Call_Expr;
      Context : not null LSP.Ada_Handlers.Context_Access)
      return LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List
   is
      Is_First_Param : Boolean := True;
      Res            :
        LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List;
      Name_Node      : constant Libadalang.Analysis.Name := C.F_Name;
      Is_Dotted_Name : constant Boolean :=
        (Name_Node.Kind in Ada_Dotted_Name_Range
         and then Name_Node.As_Dotted_Name.P_Is_Dot_Call (True));

   begin
      for N of reverse Context.Find_All_Env_Elements (Name_Node) loop
         if N.Kind in Ada_Basic_Subp_Decl
           | Ada_Null_Subp_Decl_Range
           | Ada_Expr_Function_Range
         then
            declare
               Assoc : LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data;
               Spec : constant Libadalang.Analysis.Base_Subp_Spec
                 := N.As_Basic_Decl.P_Subp_Spec_Or_Null;
            begin
               if Spec /= Libadalang.Analysis.No_Base_Subp_Spec then
                  Assoc.Title.Append ("Params of ");
                  Assoc.Title.Append
                    (VSS.Strings.To_Virtual_String (Name_Node.Text));
                  Assoc.Decl := N.As_Basic_Decl;

                  for Param of Spec.P_Params loop
                     declare
                        Param_Type : constant Type_Expr := Param.F_Type_Expr;
                     begin
                        for Id of Param.F_Ids loop
                           if not (Is_First_Param and then Is_Dotted_Name) then
                              Assoc.Param_Types.Include
                                (Id,
                                 (Node     => Param_Type.As_Ada_Node,
                                  Is_Value => False));
                              Assoc.Param_Vector.Append (Id.As_Ada_Node);
                           end if;
                           Is_First_Param := False;
                        end loop;
                     end;
                  end loop;
                  Res.Append (Assoc);
               end if;
            end;
         end if;
      end loop;

      return Res;
   end Get_Spec_Call_Expr_Designators;

   -------------------
   -- Get_Aggregate --
   -------------------

   function Get_Aggregate
     (N : Libadalang.Analysis.Ada_Node'Class)
      return Libadalang.Analysis.Aggregate is
   begin
      if N.Kind in Libadalang.Common.Ada_Aggregate_Range then
         return N.As_Aggregate;
      end if;
      return No_Aggregate;
   end Get_Aggregate;

   ---------------------
   -- Get_Designators --
   ---------------------

   function Get_Designators
     (A : Libadalang.Analysis.Aggregate)
      return Laltools.Common.Node_Vectors.Vector
   is
      Res : Laltools.Common.Node_Vectors.Vector;
   begin
      for Assoc of A.F_Assocs loop
         if Assoc.Kind in Ada_Aggregate_Assoc_Range then
            for Alt of Assoc.As_Aggregate_Assoc.F_Designators loop
               Res.Append (Alt.As_Ada_Node);
            end loop;
         end if;
      end loop;

      return Res;
   end Get_Designators;

   ------------------------------------
   -- Get_Spec_Aggregate_Designators --
   ------------------------------------

   function Get_Spec_Aggregate_Designators
     (A       : Libadalang.Analysis.Aggregate;
      Context : not null LSP.Ada_Handlers.Context_Access)
      return LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List
   is
      pragma Unreferenced (Context);
      Res       :
        LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List;
      Expr_Type : constant Base_Type_Decl := A.P_Expression_Type;
      Aggr_Type : constant Base_Type_Decl :=
        (if Expr_Type.Is_Null then No_Base_Type_Decl else Expr_Type);

      procedure Add_Component
        (Assoc : in out LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data;
         Param : Base_Formal_Param_Decl;
         Discs : Discriminant_Values_Array);
      --  Add the components if they are not a bounded discriminants.
      --  Ignore duplicated components.

      procedure Add_Discriminant
        (Assoc : in out LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data;
         Disc  : Discriminant_Values);
      --  Add the bounded discriminants

      function Get_Label_For_Shape
        (Discriminants : Discriminant_Values_Array)
         return VSS.Strings.Virtual_String;
      --  Return an unique name for a shape

      function Is_Discriminant
        (Component     : Defining_Name'Class;
         Discriminants : Discriminant_Values_Array)
         return Boolean;
      --  Return True if Component is a bounded discriminants

      -------------------
      -- Add_Component --
      -------------------

      procedure Add_Component
        (Assoc : in out LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data;
         Param : Base_Formal_Param_Decl;
         Discs : Discriminant_Values_Array)
      is
         Param_Ids  : constant Defining_Name_List :=
           (case Param.Kind is
               when Ada_Component_Decl_Range =>
                 As_Component_Decl (Param).F_Ids,
               when Ada_Discriminant_Spec    =>
                 As_Discriminant_Spec (Param).F_Ids,
               when others                   =>
                 No_Defining_Name_List);
         Param_Type : constant Type_Expr :=
           (case Param.Kind is
               when Ada_Component_Decl_Range =>
                 As_Component_Decl (Param).
                F_Component_Def.F_Type_Expr,
               when Ada_Discriminant_Spec    =>
                 As_Discriminant_Spec (Param).F_Type_Expr,
               when others                   => No_Type_Expr);
      begin
         for Id of Param_Ids loop
            --  Do nothing if this is a bounded discriminants
            if not Is_Discriminant (Id, Discs)
              --  or if the node is already present
              and then not Assoc.Param_Vector.Contains (Id.As_Ada_Node)
            then
               if not Assoc.Param_Types.Contains (Id) then
                  Assoc.Param_Types.Include
                    (Id,
                     (Node     => Param_Type.As_Ada_Node,
                      Is_Value => False));
               end if;
               Assoc.Param_Vector.Append (Id.As_Ada_Node);
            end if;
         end loop;
      end Add_Component;

      ----------------------
      -- Add_Discriminant --
      ----------------------

      procedure Add_Discriminant
        (Assoc : in out LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data;
         Disc  : Discriminant_Values)
      is
         Id : constant Libadalang.Analysis.Identifier'Class :=
           Discriminant (Disc);

         function Is_Basic_Value
           (L : Alternatives_List'Class) return Boolean;

         --------------------
         -- Is_Basic_Value --
         --------------------

         function Is_Basic_Value
           (L : Alternatives_List'Class) return Boolean is
         begin
            if L.Children_Count = 1 then
               declare
                  Value_Node : constant Ada_Node :=
                    L.Child (L.First_Child_Index);
               begin
                  case Value_Node.Kind is
                     when Ada_Bin_Op_Range | Ada_Others_Designator_Range =>
                        --  return a placeholder for "when X .. Y"
                        return False;
                     when Ada_Identifier_Range =>
                        --  return a placeholder for "when Z" with Z a type
                        return
                          Value_Node.As_Identifier
                            .P_Name_Designated_Type.Is_Null;
                     when others =>
                        return True;
                  end case;
               end;
            else
               return False;
            end if;
         end Is_Basic_Value;
      begin
         Assoc.Param_Vector.Append (Id.As_Ada_Node);
         if not Assoc.Param_Types.Contains (Id) then
            declare
               Disc_Values : constant Alternatives_List'Class := Values (Disc);
            begin
               Assoc.Param_Types.Include
                 (Id,
                  (Node     => Disc_Values.As_Ada_Node,
                   Is_Value => Is_Basic_Value (Disc_Values)));
            end;
         end if;
      end Add_Discriminant;

      -------------------------
      -- Get_Label_For_Shape --
      -------------------------

      function Get_Label_For_Shape
        (Discriminants : Discriminant_Values_Array)
         return VSS.Strings.Virtual_String
      is
         Result : VSS.Strings.Virtual_String;
         Length : constant Integer := Discriminants'Length;

      begin
         if Length = 0 then
            return
              LSP.Lal_Utils.To_Virtual_String
                ("Aggregate for " & Aggr_Type.F_Name.Text);
         end if;

         Result := "Aggregate when ";

         for Idx in Discriminants'Range loop
            declare
               Disc_Values : constant Discriminant_Values :=
                 Discriminants (Idx);
            begin
               Result.Append
                 (LSP.Lal_Utils.To_Virtual_String
                    (Discriminant (Disc_Values).Text));
               Result.Append (" => ");

               Result.Append
                 (LSP.Lal_Utils.To_Virtual_String (Values (Disc_Values).Text));

               if Idx < Discriminants'Length then
                  Result.Append (", ");
               end if;
            end;
         end loop;

         return Result;
      end Get_Label_For_Shape;

      ---------------------
      -- Is_Discriminant --
      ---------------------

      function Is_Discriminant
        (Component     : Defining_Name'Class;
         Discriminants : Discriminant_Values_Array)
         return Boolean is
      begin
         for Disc of Discriminants loop
            if Discriminant (Disc).Text = Component.Text then
               return True;
            end if;
         end loop;
         return False;
      end Is_Discriminant;

   begin
      --  If the aggregate node has no type (e.g: representation clauses),
      --  return immediately.
      if Expr_Type.Is_Null then
         return Res;
      end if;

      if Aggr_Type.Kind in Ada_Type_Decl then
         declare
            Base_Type  : constant Base_Type_Decl :=
              Aggr_Type.P_Base_Type (Origin => A);
            Is_Private : constant Boolean :=
              not Base_Type.Is_Null and then Base_Type.P_Is_Private;
         begin

            for Shape of reverse Aggr_Type.As_Type_Decl.P_Shapes
              (Include_Discriminants => True,
               --  This is mandatory to retrieve unbounded discriminants
               Origin                => A)
            loop
               declare
                  Assoc         :
                    LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data;
                  Discriminants : constant Discriminant_Values_Array :=
                    Discriminants_Values (Shape);
                  --  Discriminants_Values only returns the discrimants
                  --  with bounded values
                  Components    : constant Base_Formal_Param_Decl_Array :=
                    Libadalang.Analysis.Components (Shape);
                  --  Components contains all the components + all the
                  --  Discriminants (P_Shapes was called with
                  --  Include_Discriminants => True)
               begin
                  Assoc.Decl := Aggr_Type.As_Basic_Decl;
                  Assoc.Title := Get_Label_For_Shape (Discriminants);

                  --  If we are dealing with a derived type that does not have
                  --  access to its parent full view, we should use the
                  --  extension aggregate notation (see RM 4.3.2 for more
                  --  info).
                  if Is_Private then
                     Assoc.Prefix.Append
                       (VSS.Strings.To_Virtual_String (Base_Type.F_Name.Text));
                     Assoc.Prefix.Append (" with ");
                  end if;

                  --  Add the bounded discriminant first
                  for Disc of Discriminants loop
                     Add_Discriminant (Assoc, Disc);
                  end loop;

                  declare
                     All_Components : constant Base_Formal_Param_Decl_Array :=
                       (if Is_Private
                        then Base_Type.P_Discriminants_List & Components
                        --  This union can have duplicates: add the parent
                        --  discriminants first
                        else Components);
                  begin
                     for Comp of All_Components loop
                        Add_Component (Assoc, Comp, Discriminants);
                     end loop;
                  end;
                  Res.Append (Assoc);
               end;
            end loop;
         end;
      end if;

      return Res;
   end Get_Spec_Aggregate_Designators;

   -------------------------
   -- Get_Generic_Package --
   -------------------------

   function Get_Generic_Package
     (N : Libadalang.Analysis.Ada_Node'Class)
      return Libadalang.Analysis.Generic_Package_Instantiation
   is
   begin
      if N.Kind in Ada_Generic_Package_Instantiation_Range then
         return N.As_Generic_Package_Instantiation;
      end if;

      declare
         N_Parent : constant Libadalang.Analysis.Ada_Node'Class := N.Parent;
      begin
         if not N_Parent.Is_Null
           and then N_Parent.Kind in Ada_Generic_Package_Instantiation_Range
         then
            return N_Parent.As_Generic_Package_Instantiation;
         end if;
      end;
      return No_Generic_Package_Instantiation;
   end Get_Generic_Package;

   ---------------------
   -- Get_Designators --
   ---------------------

   function Get_Designators
     (G : Libadalang.Analysis.Generic_Package_Instantiation)
      return Laltools.Common.Node_Vectors.Vector
   is
      Designator : Libadalang.Analysis.Ada_Node;
      Res        : Laltools.Common.Node_Vectors.Vector;
   begin
      for Assoc of G.F_Params loop
         Designator := Assoc.As_Param_Assoc.F_Designator;
         if Designator /= No_Ada_Node then
            Res.Append (Designator);
         end if;
      end loop;

      return Res;
   end Get_Designators;

   --------------------------
   -- Get_Decl_Designators --
   --------------------------

   function Get_Decl_Designators
     (G       : Libadalang.Analysis.Generic_Package_Instantiation;
      Context : not null LSP.Ada_Handlers.Context_Access)
      return LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List
   is
      Res       :
        LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data_Lists.List;
      Name_Node : constant Libadalang.Analysis.Name := G.F_Generic_Pkg_Name;
   begin
      for N of reverse Context.Find_All_Env_Elements (Name_Node) loop
         if N.Kind in Ada_Generic_Package_Decl_Range then
            declare
               Assoc : LSP.Ada_Completions.Generic_Assoc_Utils.Assoc_Data;
               Decl  : constant Libadalang.Analysis.Generic_Package_Decl
                 := N.As_Generic_Package_Decl;
            begin
               Assoc.Title.Append ("Params of ");
               Assoc.Title.Append
                 (VSS.Strings.To_Virtual_String (Name_Node.Text));
               Assoc.Decl := N.As_Basic_Decl;

               for Param of Decl.F_Formal_Part.F_Decls loop
                  if Param.Kind in Ada_Generic_Formal then
                     declare
                        Gen_Formal : constant Generic_Formal :=
                          Param.As_Generic_Formal;
                        Param_Name : constant Name :=
                          Gen_Formal.F_Decl.P_Defining_Name.F_Name;
                     begin
                        Assoc.Param_Types.Include
                          (Gen_Formal.F_Decl.P_Defining_Name.F_Name,
                           (Node     => Gen_Formal.As_Ada_Node,
                            Is_Value => False));
                        Assoc.Param_Vector.Append (Param_Name.As_Ada_Node);
                     end;
                  end if;
               end loop;
               Res.Append (Assoc);
            end;
         end if;
      end loop;

      return Res;
   end Get_Decl_Designators;

   ---------------------
   -- Get_Prefix_Node --
   ---------------------

   function Get_Prefix_Node
     (C      : Libadalang.Analysis.Call_Expr;
      Column : out Langkit_Support.Slocs.Column_Number)
      return Libadalang.Analysis.Ada_Node'Class is
   begin
      Column := C.Parent.Sloc_Range.Start_Column;
      return C.Parent;
   end Get_Prefix_Node;

   ---------------------
   -- Get_Prefix_Node --
   ---------------------

   function Get_Prefix_Node
     (A      : Libadalang.Analysis.Aggregate;
      Column : out Langkit_Support.Slocs.Column_Number)
      return Libadalang.Analysis.Ada_Node'Class is
   begin
      Column := A.Parent.Sloc_Range.Start_Column;
      return A.Parent;
   end Get_Prefix_Node;

   ----------------
   -- Get_Prefix --
   ----------------

   function Get_Prefix_Node
     (G      : Libadalang.Analysis.Generic_Package_Instantiation;
      Column : out Langkit_Support.Slocs.Column_Number)
      return Libadalang.Analysis.Ada_Node'Class is
   begin
      Column := G.Sloc_Range.Start_Column;
      return G.F_Generic_Pkg_Name;
   end Get_Prefix_Node;

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   : Parameter_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Messages.CompletionList)
   is
      Count        : Natural := 0;
      Unsorted_Res : LSP.Messages.CompletionItem_Vector;
   begin
      Call_Expr_Completion.Propose_Completion
        (Self         => Self,
         Sloc         => Sloc,
         Token        => Token,
         Node         => Node,
         Limit        => Self.Named_Notation_Threshold,
         Filter       => Filter,
         Names        => Names,
         Unsorted_Res => Unsorted_Res);

      Aggregate_Completion.Propose_Completion
        (Self         => Self,
         Sloc         => Sloc,
         Token        => Token,
         Node         => Node,
         Limit        => Self.Named_Notation_Threshold,
         Filter       => Filter,
         Names        => Names,
         Unsorted_Res => Unsorted_Res);

      Generic_Package_Completion.Propose_Completion
        (Self         => Self,
         Sloc         => Sloc,
         Token        => Token,
         Node         => Node,
         Limit        => Self.Named_Notation_Threshold,
         Filter       => Filter,
         Names        => Names,
         Unsorted_Res => Unsorted_Res);

      declare
         Min_Width : constant Natural := Unsorted_Res.Length'Img'Length - 1;
      begin
         for Unsort_Item of reverse Unsorted_Res loop
            --  Use a "+" as the first sorted character to be shown before
            --  the items from the other providers ("+" is lower than
            --  the alphanumeric symbol and "~" in the ASCII table)
            Unsort_Item.sortText := (True, Value => <>);
            Unsort_Item.sortText.Value.Append ('+');
            Unsort_Item.sortText.Value.Append
              (VSS.Strings.Conversions.To_Virtual_String
                 (GNATCOLL.Utils.Image (Count, Min_Width => Min_Width)));
            Result.items.Append (Unsort_Item);
            Count := Count + 1;
         end loop;
      end;
   end Propose_Completion;

end LSP.Ada_Completions.Parameters;
