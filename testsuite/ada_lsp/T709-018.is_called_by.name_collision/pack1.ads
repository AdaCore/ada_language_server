with Common;

package Pack1 is

   type Data1 is new Common.Data with null record;

   overriding function Create (Self : access Data1) return Integer;

end Pack1;
