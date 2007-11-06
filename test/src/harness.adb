with AUnit;
with Savadur_Suite;

-------------
-- Harness --
-------------

procedure Harness is

   procedure Run is new AUnit.Test_Runner (Savadur_Suite.Suite);

begin
   Run;
end Harness;
