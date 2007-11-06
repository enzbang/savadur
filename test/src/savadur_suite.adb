with AUnit; use AUnit.Test_Suites;

with Config_Parse;

package body Savadur_Suite is

   Result : aliased Test_Suite;
   Test_1 : aliased Config_Parse.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end Savadur_Suite;
