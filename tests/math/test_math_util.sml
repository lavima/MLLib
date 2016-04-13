(* 
* file: test_math_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the MathUtil structure.
*)

val _ = print"\n\n********** MathUtil Tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="MathUtil", what="Testing MathUtil.blerp",
    genInput= fn() => [ ( 1.0, 9.0, 2.0, 4.0, 0.5, 0.5 ) ] ,
    f= fn[ i1 ] => [ MathUtil.blerp i1 ] ,
    evaluate= fn[ o1 ] => [ Util.approxEqReal( o1, 4.0, 6 ) ] ,
    inputToString=
      fn( x, y, z, u, v, w ) =>
        "( " ^
        Real.toString x ^ ", " ^ 
        Real.toString y ^ ", " ^ 
        Real.toString z ^ ", " ^ 
        Real.toString u ^ ", " ^ 
        Real.toString v ^ ", " ^ 
        Real.toString w ^ 
        " )" }
