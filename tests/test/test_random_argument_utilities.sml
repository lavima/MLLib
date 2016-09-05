(* 
* file: test_random_argumment_utilities.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains code to test the random argument utilities
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="RandomArgumentUtilities", what="randomBSRImage",
    genInput= fn () => [ () ] ,
    f= fn[ i1 ] => [ RandomArgumentUtilities.randomBSRImage() ] ,
    evaluate= fn[ o1 ] => [ true ] ,
    inputToString= fn _ => "unit" }
