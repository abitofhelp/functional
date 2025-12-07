pragma Ada_2022;
--  ===========================================================================
--  SPARK_Instantiations Body
--  ===========================================================================

package body SPARK_Instantiations with
  SPARK_Mode => On
is

   --  ========================================================================
   --  Option Tests
   --  ========================================================================

   function Test_Option_Some return Int_Option.Option is
   begin
      return Int_Option.New_Some (42);
   end Test_Option_Some;

   function Test_Option_None return Int_Option.Option is
   begin
      return Int_Option.None;
   end Test_Option_None;

   --  ========================================================================
   --  Result Tests
   --  ========================================================================

   function Test_Result_Ok return Int_Result.Result is
   begin
      return Int_Result.Ok (42);
   end Test_Result_Ok;

   function Test_Result_Error return Int_Result.Result is
   begin
      return Int_Result.New_Error (Validation_Failed);
   end Test_Result_Error;

   --  ========================================================================
   --  Either Tests
   --  ========================================================================

   function Test_Either_Left return String_Or_Int.Either is
      Msg : constant Error_Message := (others => ' ');
   begin
      return String_Or_Int.Left (Msg);
   end Test_Either_Left;

   function Test_Either_Right return String_Or_Int.Either is
   begin
      return String_Or_Int.Right (42);
   end Test_Either_Right;

end SPARK_Instantiations;
