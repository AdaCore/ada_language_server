--  Copyright <YEAR> <COPYRIGHT HOLDER>
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.

with Ada.Unchecked_Deallocation;

package body LSP_Gen.Entities is
   procedure Free is new Ada.Unchecked_Deallocation
     (Property_Array, Property_Array_Access);

   overriding procedure Adjust (Self : in out Property_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Property_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Property_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Property_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Property_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Property_Vector; Value : Property) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Property'Size);
      Self_Data_Saved : Property_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Property_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Property_Array'
             (Self.Data.all & Property_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Property_Variable_Reference
     (Self  : aliased in out Property_Vector;
      Index : Positive)
      return Property_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Property_Constant_Reference
     (Self  : aliased Property_Vector;
      Index : Positive)
      return Property_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Request_Array, Request_Array_Access);

   overriding procedure Adjust (Self : in out Request_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Request_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Request_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Request_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Request_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Request_Vector; Value : Request) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Request'Size);
      Self_Data_Saved : Request_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Request_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Request_Array'
             (Self.Data.all & Request_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Request_Variable_Reference
     (Self  : aliased in out Request_Vector;
      Index : Positive)
      return Request_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Request_Constant_Reference
     (Self  : aliased Request_Vector;
      Index : Positive)
      return Request_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Integer_Array, Integer_Array_Access);

   overriding procedure Adjust (Self : in out Integer_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Integer_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Integer_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Integer_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Integer_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Integer_Vector; Value : Integer) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Integer'Size);
      Self_Data_Saved : Integer_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Integer_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Integer_Array'
             (Self.Data.all & Integer_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Integer_Variable_Reference
     (Self  : aliased in out Integer_Vector;
      Index : Positive)
      return Integer_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Integer_Constant_Reference
     (Self  : aliased Integer_Vector;
      Index : Positive)
      return Integer_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (TypeAlias_Array, TypeAlias_Array_Access);

   overriding procedure Adjust (Self : in out TypeAlias_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new TypeAlias_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out TypeAlias_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : TypeAlias_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out TypeAlias_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out TypeAlias_Vector; Value : TypeAlias) is
      Init_Length     : constant Positive      :=
        Positive'Max (1, 256 / TypeAlias'Size);
      Self_Data_Saved : TypeAlias_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new TypeAlias_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new TypeAlias_Array'
             (Self.Data.all & TypeAlias_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_TypeAlias_Variable_Reference
     (Self  : aliased in out TypeAlias_Vector;
      Index : Positive)
      return TypeAlias_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_TypeAlias_Constant_Reference
     (Self  : aliased TypeAlias_Vector;
      Index : Positive)
      return TypeAlias_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Structure_Array, Structure_Array_Access);

   overriding procedure Adjust (Self : in out Structure_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Structure_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Structure_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Structure_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Structure_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Structure_Vector; Value : Structure) is
      Init_Length     : constant Positive      :=
        Positive'Max (1, 256 / Structure'Size);
      Self_Data_Saved : Structure_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Structure_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Structure_Array'
             (Self.Data.all & Structure_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Structure_Variable_Reference
     (Self  : aliased in out Structure_Vector;
      Index : Positive)
      return Structure_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Structure_Constant_Reference
     (Self  : aliased Structure_Vector;
      Index : Positive)
      return Structure_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Notification_Array, Notification_Array_Access);

   overriding procedure Adjust (Self : in out Notification_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Notification_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Notification_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Notification_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out Notification_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out Notification_Vector; Value : Notification) is
      Init_Length     : constant Positive         :=
        Positive'Max (1, 256 / Notification'Size);
      Self_Data_Saved : Notification_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Notification_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Notification_Array'
             (Self.Data.all & Notification_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Notification_Variable_Reference
     (Self  : aliased in out Notification_Vector;
      Index : Positive)
      return Notification_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Notification_Constant_Reference
     (Self  : aliased Notification_Vector;
      Index : Positive)
      return Notification_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (EnumerationEntry_Array, EnumerationEntry_Array_Access);

   overriding procedure Adjust (Self : in out EnumerationEntry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new EnumerationEntry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out EnumerationEntry_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : EnumerationEntry_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out EnumerationEntry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out EnumerationEntry_Vector; Value : EnumerationEntry) is
      Init_Length     : constant Positive             :=
        Positive'Max (1, 256 / EnumerationEntry'Size);
      Self_Data_Saved : EnumerationEntry_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new EnumerationEntry_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new EnumerationEntry_Array'
             (Self.Data.all & EnumerationEntry_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_EnumerationEntry_Variable_Reference
     (Self  : aliased in out EnumerationEntry_Vector;
      Index : Positive)
      return EnumerationEntry_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_EnumerationEntry_Constant_Reference
     (Self  : aliased EnumerationEntry_Vector;
      Index : Positive)
      return EnumerationEntry_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Enumeration_Array, Enumeration_Array_Access);

   overriding procedure Adjust (Self : in out Enumeration_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Enumeration_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Enumeration_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Enumeration_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Enumeration_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Enumeration_Vector; Value : Enumeration) is
      Init_Length     : constant Positive        :=
        Positive'Max (1, 256 / Enumeration'Size);
      Self_Data_Saved : Enumeration_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Enumeration_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Enumeration_Array'
             (Self.Data.all & Enumeration_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Enumeration_Variable_Reference
     (Self  : aliased in out Enumeration_Vector;
      Index : Positive)
      return Enumeration_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Enumeration_Constant_Reference
     (Self  : aliased Enumeration_Vector;
      Index : Positive)
      return Enumeration_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (AType_Array, AType_Array_Access);

   overriding procedure Adjust (Self : in out AType_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new AType_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out AType_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : AType_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out AType_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out AType_Vector; Value : AType) is
      Init_Length : constant Positive  := Positive'Max (1, 256 / AType'Size);
      Self_Data_Saved : AType_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new AType_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new AType_Array'
             (Self.Data.all & AType_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_AType_Variable_Reference
     (Self  : aliased in out AType_Vector;
      Index : Positive)
      return AType_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_AType_Constant_Reference
     (Self  : aliased AType_Vector;
      Index : Positive)
      return AType_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Element
     (Self : aliased in out AType_Holder) return AType_Variable_Reference is
   begin
      if Self.Length = 0 then
         Self.Append ((others => <>));
      end if;
      return (Element => Self.Data (1)'Access);
   end Element;

   not overriding function Value
     (Self : aliased AType_Holder) return AType_Constant_Reference is
     (Element => Self.Data (1)'Access);

end LSP_Gen.Entities;
