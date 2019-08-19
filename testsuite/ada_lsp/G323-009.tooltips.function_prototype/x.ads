package X is

   function F1 return not null access String;
   -- yo

   function F2 (Z : access String := null) return String;

   function F3 (Z : Boolean := not True) return String;

   function F4 (Z : Boolean := not True) return access String;

   function F5 (Z : Boolean := not True) return not null access String;

   function F6 return not null access String;

   function F7 (Z : Boolean := (True or True) and not False) return String;
   function F8 (Z : Boolean := (True or True) and not False and False) return String;

end X;
