with AUnit;

package Config_Parse is

   use AUnit;
   use AUnit.Test_Cases;
   use AUnit.Test_Results;
   use AUnit.Message_Strings;

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case)
                  return Test_String;
   --  Returns name identifying the test case

end Config_Parse;
