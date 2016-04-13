(*
* filename: test_randomTest.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* Some
*)

val _ = print"\n\n********** Random Tests tests **********\n"

val _ = DifferentialTest.test' ( CommandLine.arguments() ) { 
  group = "RandomTest",
  what = "Test_sine_no_error",

  num = 3,
  genInput = (fn i => 3.3),

  fs = [ Math.sin, Math.sin ],

  compare = Real.==,
  inputToString = Real.toString
}
