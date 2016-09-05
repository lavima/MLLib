(* 
* file: test_math_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the MathUtil structure.
*)

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

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="MathUtil", what="Testing MathUtil.chiSquared",
    genInput= fn() => [ ( Array.fromList [ 0.1, 0.2, 0.3, 0.4 ], 
                          Array.fromList [ 0.4, 0.3, 0.2, 0.1 ] ) ] ,
    f= fn[ i1 ] => [ MathUtil.chiSquared i1 ] ,
    evaluate= fn[ o1 ] => [ Util.approxEqReal( o1, 0.2, 6 ) ] ,
    inputToString=
      fn( x, y ) => 
        "( " ^ ( ArrayUtil.toString Real.toString x ) ^ ", " ^
               ( ArrayUtil.toString Real.toString y ) ^ " )" }
