
with G;
package Inst is
   type String_Ptr is access all String;
   package I is new G (String, String_Ptr);
end Inst;

