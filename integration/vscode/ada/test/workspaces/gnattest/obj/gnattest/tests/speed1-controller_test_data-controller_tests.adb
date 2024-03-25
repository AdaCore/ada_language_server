--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Speed1.Controller_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Speed1.Controller_Test_Data.Controller_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Speed (Gnattest_T : in out Test_Controller);
   procedure Test_Speed_bdc804 (Gnattest_T : in out Test_Controller) renames Test_Speed;
--  id:2.2/bdc8045e732efa1b/Speed/1/0/
   procedure Test_Speed (Gnattest_T : in out Test_Controller) is
   --  speed1.ads:12:4:Speed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      null;
      --  AUnit.Assertions.Assert
      --    (Gnattest_Generated.Default_Assert_Value,
      --     "Test not implemented.");

--  begin read only
   end Test_Speed;
--  end read only


--  begin read only
   procedure Test_Adjust_Speed (Gnattest_T : in out Test_Controller);
   procedure Test_Adjust_Speed_6fd48f (Gnattest_T : in out Test_Controller) renames Test_Adjust_Speed;
--  id:2.2/6fd48ff933c1edff/Adjust_Speed/1/0/
   procedure Test_Adjust_Speed (Gnattest_T : in out Test_Controller) is
   --  speed1.ads:13:4:Adjust_Speed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      null;
      --  delay 5.0;
      --  AUnit.Assertions.Assert
      --    (Adjust_Speed (),
      --     "Test not implemented.");

--  begin read only
   end Test_Adjust_Speed;
--  end read only


--  begin read only
   --  procedure Test_Adjust_Speed (Gnattest_T : in out Test_Controller);
   --  procedure Test_Adjust_Speed_cea401 (Gnattest_T : in out Test_Controller) renames Test_Adjust_Speed;
--  id:2.2/cea4013b78a4a615/Adjust_Speed/0/1/
   --  procedure Test_Adjust_Speed (Gnattest_T : in out Test_Controller) is
--  end read only
--
--        pragma Unreferenced (Gnattest_T);
--
--     begin
--
--        AUnit.Assertions.Assert
--          (Gnattest_Generated.Default_Assert_Value,
--           "Test not implemented.");
--
--  begin read only
   --  end Test_Adjust_Speed;
--  end read only


--  begin read only
   --  procedure Test_Adjust_Speed (Gnattest_T : in out Test_Controller);
   --  procedure Test_Adjust_Speed_d4219e (Gnattest_T : in out Test_Controller) renames Test_Adjust_Speed;
--  id:2.2/d4219e26c5c99d2c/Adjust_Speed/0/1/
   --  procedure Test_Adjust_Speed (Gnattest_T : in out Test_Controller) is
--  end read only
--
--        pragma Unreferenced (Gnattest_T);
--
--     begin
--
--        AUnit.Assertions.Assert
--          (Gnattest_Generated.Default_Assert_Value,
--           "Test not implemented.");
--
--  begin read only
   --  end Test_Adjust_Speed;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Speed1.Controller_Test_Data.Controller_Tests;
