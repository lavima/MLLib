(* 
* file: test_math_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the MathUtil structure.
*)

val _ = print"\n\n********** MathUtil Tests **********\n"

val _ = UnitTest.test( "Testing MathUtil.blerp",
  fn() => 
    MathUtil.blerp( 1.0, 9.0, 2.0, 4.0, 0.5, 0.5 ) ,
  fn X =>
    Util.approxEqReal( X, 4.0, 6 ) )
