(* 
* file: test_basic_transformations.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains code to test basic transformations
*)


val _ = print"\n\n********** Basic transformations tests **********\n"

val _ = UnitTest.test( "range to range",
  fn() => BasicTransformations.rangeToRange ((10.0, 100.0), (~20.0, 40.0)) 20.0,
  fn x =>
     Util.approxEqReal (~13.3333333333, x, 5)
  )
  ;
