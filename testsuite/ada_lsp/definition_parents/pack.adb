package body pack is
   
   overriding procedure Method (X : Child) is null;
   
   overriding procedure Method (X : Other_Grandchild) is
      C : Child;
   begin
      C.Method;
   end Method;
   
end pack;
