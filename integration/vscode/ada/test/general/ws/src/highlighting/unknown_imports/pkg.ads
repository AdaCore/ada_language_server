--  This test intends to check that unknown packages do not
--  get semantic tokens assigned to them
with Ada.Text_IO;
with Unknown.Pkg;

package Pkg is
   pragma Preelaborate;
end Pkg;
