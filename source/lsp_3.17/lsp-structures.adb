--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

package body LSP.Structures is

   procedure Free is new Ada.Unchecked_Deallocation
     (DocumentSymbol_Array, DocumentSymbol_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (SelectionRange_Array, SelectionRange_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (SelectionRange, SelectionRange_Access);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out DocumentSymbol_Vector) is
   begin
      if Self.Data /= null then
         Self.Data := new DocumentSymbol_Array'(Self.Data.all);
      end if;
   end Adjust;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out SelectionRange_Optional) is
   begin
      if Self.Value /= null then
         Self.Value := new SelectionRange'(Self.Value.all);
      end if;
   end Adjust;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out SelectionRange_Vector) is
   begin
      if Self.Data /= null then
         Self.Data := new SelectionRange_Array'(Self.Data.all);
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self  : in out DocumentSymbol_Vector;
      Value : DocumentSymbol)
   is
      Old : DocumentSymbol_Array_Access := Self.Data;
   begin
      if Old = null then
         Self.Data := new DocumentSymbol_Array'(1 => Value);
      else
         Self.Data := new DocumentSymbol_Array'(Old.all & Value);
         Free (Old);
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self  : in out SelectionRange_Vector;
      Value : SelectionRange)
   is
      Old : SelectionRange_Array_Access := Self.Data;
   begin
      if Old = null then
         Self.Data := new SelectionRange_Array'(1 => Value);
      else
         Self.Data := new SelectionRange_Array'(Old.all & Value);
         Free (Old);
      end if;
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out DocumentSymbol_Vector) is
   begin
      Free (Self.Data);
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out SelectionRange_Optional) is
   begin
      Free (Self.Value);
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out SelectionRange_Vector) is
   begin
      Free (Self.Data);
   end Clear;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out DocumentSymbol_Vector) is
   begin
      Free (Self.Data);
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out SelectionRange_Vector) is
   begin
      Free (Self.Data);
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out SelectionRange_Optional) is
   begin
      Free (Self.Value);
   end Finalize;

   -------------------------------------------
   -- Get_DocumentSymbol_Constant_Reference --
   -------------------------------------------

   function Get_DocumentSymbol_Constant_Reference
     (Self : aliased DocumentSymbol_Vector; Index : Positive)
      return DocumentSymbol_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_DocumentSymbol_Constant_Reference;

   -------------------------------------------
   -- Get_DocumentSymbol_Variable_Reference --
   -------------------------------------------

   function Get_DocumentSymbol_Variable_Reference
     (Self : aliased in out DocumentSymbol_Vector; Index : Positive)
      return DocumentSymbol_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_DocumentSymbol_Variable_Reference;

   -------------------------------------------
   -- Get_SelectionRange_Constant_Reference --
   -------------------------------------------

   function Get_SelectionRange_Constant_Reference
     (Self : aliased SelectionRange_Vector; Index : Positive)
      return SelectionRange_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_SelectionRange_Constant_Reference;

   -------------------------------------------
   -- Get_SelectionRange_Variable_Reference --
   -------------------------------------------

   function Get_SelectionRange_Variable_Reference
     (Self : aliased in out SelectionRange_Vector; Index : Positive)
      return SelectionRange_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_SelectionRange_Variable_Reference;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Self : SelectionRange_Optional) return Boolean is
   begin
      return Self.Value /= null;
   end Is_Set;

   ------------
   -- Length --
   ------------

   function Length (Self : DocumentSymbol_Vector) return Natural is
   begin
      return (if Self.Data = null then 0 else Self.Data'Length);
   end Length;

   ------------
   -- Length --
   ------------

   function Length (Self : SelectionRange_Vector) return Natural is
   begin
      return (if Self.Data = null then 0 else Self.Data'Length);
   end Length;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out SelectionRange_Optional;
      Value : SelectionRange) is
   begin
      if Self.Value = null then
         Self.Value := new SelectionRange'(Value);
      else
         Self.Value.all := Value;
      end if;
   end Set;

   -----------
   -- Value --
   -----------

   function Value (Self : SelectionRange_Optional) return SelectionRange is
   begin
      return Self.Value.all;
   end Value;

end LSP.Structures;
