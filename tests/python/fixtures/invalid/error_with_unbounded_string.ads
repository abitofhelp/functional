-- Invalid: Error type using unbounded String
package Domain.Error is
   type Error_Code is (Success, Failure);

   type Domain_Error is record
      Code    : Error_Code;
      Message : String;  -- ❌ Should use Bounded_String
   end record;
end Domain.Error;
