package A.B is
   procedure Run;
end A.B;

package body A.B is
   procedure Run is separate;
end A.B;

generic
package Generic_Pkg is
end Generic_Pkg;

with Generic_Pkg;
package Instance_Pkg is new Generic_Pkg;

with A.B;
package Alias_Pkg renames A.B;

procedure Main is
begin
   null;
end Main;
