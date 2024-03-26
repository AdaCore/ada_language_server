--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Speed1.Controller_Test_Data.Controller_Tests;

with GNATtest_Generated;

package Speed2.Auto_Controller_Test_Data is

--  begin read only
   type Test_Auto_Controller is new
     GNATtest_Generated.GNATtest_Standard.Speed1.Controller_Test_Data.Controller_Tests.Test_Controller
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test_Auto_Controller);
   procedure Tear_Down (Gnattest_T : in out Test_Auto_Controller);

end Speed2.Auto_Controller_Test_Data;
