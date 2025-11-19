-- Invalid: Production code importing test framework
with Test_Framework;  -- ❌ Production code must not import test frameworks

package Application.Service is
   procedure Execute;
end Application.Service;
