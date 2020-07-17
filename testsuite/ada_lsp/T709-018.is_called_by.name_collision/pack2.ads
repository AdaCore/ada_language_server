with Common;

package Pack2 is

   type Data2 is new Common.Data with null record;

   overriding function Create (Self : access Data2) return Integer;

end Pack2;
