--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Speed2.Auto_Controller_Test_Data is

   Local_Auto_Controller : aliased GNATtest_Generated.GNATtest_Standard.Speed2.Auto_Controller;
   procedure Set_Up (Gnattest_T : in out Test_Auto_Controller) is
   begin
      GNATtest_Generated.GNATtest_Standard.Speed1.Controller_Test_Data.Controller_Tests.Set_Up
        (GNATtest_Generated.GNATtest_Standard.Speed1.Controller_Test_Data.Controller_Tests.Test_Controller (Gnattest_T));
      Gnattest_T.Fixture := Local_Auto_Controller'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Auto_Controller) is
   begin
      GNATtest_Generated.GNATtest_Standard.Speed1.Controller_Test_Data.Controller_Tests.Tear_Down
        (GNATtest_Generated.GNATtest_Standard.Speed1.Controller_Test_Data.Controller_Tests.Test_Controller (Gnattest_T));
   end Tear_Down;

end Speed2.Auto_Controller_Test_Data;
