(* 
* file: test_random_argumment_utilities.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains code to test the random argument utilities
*)


val _ = print"\n\n********** Random argument utilities tests **********\n"

val _ = UnitTest.test( "random BSR image",
  fn() => RandomArgumentUtilities.randomBSRImage (),
  fn x =>
     let
       val _ = print x
     in
       true
     end
  )
  ;
