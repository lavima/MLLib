(*
* filename: test_randomTest.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* Some
*)

val _ = print"\n\n********** Random Tests tests **********\n"

val _ = RandomTest.runTest(
  { 
  group = "RandomTest",
  what = "Test_sine_no_error",

  numberOfTests = 3,
  testGenerator = (fn () => 3.3),

  implementations = [ Math.sin, Math.sin ],

  compareResult = Real.==,
  argsToString = Real.toString
})
