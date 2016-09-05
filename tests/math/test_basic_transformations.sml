(* 
* file: test_basic_transformations.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains code to test basic transformations
*)


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="BasicTransformations", what="range to range",
    genInput= fn() => [ ( ( ( 10.0, 100.0 ), ( ~20.0, 40.0 ) ), 20.0 ) ] ,
    f= fn[ i1 ] => [ BasicTransformations.rangeToRange ( #1 i1 ) ( #2 i1 ) ] ,
    evaluate= fn[ o1 ] => [ Util.approxEqReal ( ~13.3333333333, o1, 5 ) ] ,
    inputToString= 
      fn( ( ( x, y ), ( u, v ) ), w ) =>
        "( ( ( " ^
        Real.toString x ^ ", " ^
        Real.toString y ^ " ), ( " ^
        Real.toString u ^ ", " ^
        Real.toString v ^ " ) ), " ^
        Real.toString w ^ " )" }

