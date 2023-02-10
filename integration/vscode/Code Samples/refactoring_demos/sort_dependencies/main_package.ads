pragma Ada_2012;

--  Comment 1
with Ada.Containers; --  Comment 2
--  Comment 3
private with Ada.Finalization; --  Comment 4
--  Comment 5
with Ada.Strings.Unbounded; --  Comment 6
private with Ada.Unchecked_Deallocation; --  Comment 7
--  Comment 8

--  Comment 9
--  Comment 10
private with Qux; --  Comment 11
pragma Elaborate (Qux); -- Comment 12

--  Comment 13
--  Comment 14

--  Comment 15

--  Comment 16
--  Comment 17
--  Comment 18
with Foo; --  Comment 19
pragma Elaborate_All (Foo);
pragma Warnings (Off, --  Comment 20
                 Foo);     -- Comment 21
-- Comment 22

limited with Corge;
--  Comment 23
with Bar; use Bar; --  Comment 24
--  Comment 25
pragma Warnings (Off, --  Comment 26
                 Bar);

use Foo;
--  Comment 27
--  Comment 28

with Ada.Strings;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

with Ada.Strings.Hash;
--  Comment 29
--  Comment 30



package Main_Package is
   pragma Elaborate_Body;

end Main_Package;
