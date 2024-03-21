--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

package Speed1.Controller_Test_Data is

   type Controller_Access is access all GNATtest_Generated.GNATtest_Standard.Speed1.Controller'Class;

--  begin read only
   type Test_Controller is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Controller_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_Controller);
   procedure Tear_Down (Gnattest_T : in out Test_Controller);

end Speed1.Controller_Test_Data;
