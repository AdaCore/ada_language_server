--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Speed2.Auto_Controller_Test_Data.

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
package body Speed2.Auto_Controller_Test_Data.Auto_Controller_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Desired_Speed (Gnattest_T : in out Test_Auto_Controller);
   procedure Test_Desired_Speed_3a9813 (Gnattest_T : in out Test_Auto_Controller) renames Test_Desired_Speed;
--  id:2.2/3a98136ae8d1ca89/Desired_Speed/1/0/
   procedure Test_Desired_Speed (Gnattest_T : in out Test_Auto_Controller) is
   --  speed2.ads:10:4:Desired_Speed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      null;
      --  delay 10.0;
      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Desired_Speed;
--  end read only


--  begin read only
   procedure Test_Set_Desired_Speed (Gnattest_T : in out Test_Auto_Controller);
   procedure Test_Set_Desired_Speed_42cd33 (Gnattest_T : in out Test_Auto_Controller) renames Test_Set_Desired_Speed;
--  id:2.2/42cd33c8ea29e2bf/Set_Desired_Speed/1/0/
   procedure Test_Set_Desired_Speed (Gnattest_T : in out Test_Auto_Controller) is
   --  speed2.ads:12:4:Set_Desired_Speed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      null;
      --  delay 4.0;
      --  AUnit.Assertions.Assert
      --    (Gnattest_Generated.Default_Assert_Value,
      --     "Test not implemented.");

--  begin read only
   end Test_Set_Desired_Speed;
--  end read only


--  begin read only
   procedure Test_Adjust_Speed (Gnattest_T : in out Test_Auto_Controller);
   procedure Test_Adjust_Speed_6fd48f (Gnattest_T : in out Test_Auto_Controller) renames Test_Adjust_Speed;
--  id:2.2/6fd48ff933c1edff/Adjust_Speed/1/0/
   procedure Test_Adjust_Speed (Gnattest_T : in out Test_Auto_Controller) is
   --  speed2.ads:16:4:Adjust_Speed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      null;
      --  AUnit.Assertions.Assert
      --    (Gnattest_Generated.Default_Assert_Value,
      --     "Test not implemented.");

--  begin read only
   end Test_Adjust_Speed;
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
end Speed2.Auto_Controller_Test_Data.Auto_Controller_Tests;
