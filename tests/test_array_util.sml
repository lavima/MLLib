(* 
* file: test_array_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the functionality in the ArrayUtil 
* structure.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ArrayUtil", what="allEq",
    genInput= 
      fn() => [ 
        ( Array.fromList[ 0, 1, 2, 3, 4, 5 ], 
          Array.fromList[] ),
        ( Array.fromList[ 0, 1, 2, 3, 4, 5 ], 
          Array.fromList[ 0, 1, 2, 3, 5, 4 ] ),
        ( Array.fromList[ 0, 1, 2, 3, 4, 5 ], 
          Array.fromList[ 0, 1, 2, 3, 4, 5 ] ) ],
    f= 
      fn[ i1, i2, i3 ] => [ 
        ArrayUtil.allEq Util.eqInt i1,
        ArrayUtil.allEq Util.eqInt i2,
        ArrayUtil.allEq Util.eqInt i3 ],
    evaluate= 
      fn[ o1, o2, o3 ] =>
        [ not o1, not o2, o3 ] ,
    inputToString= 
      fn( a1, a2 ) => 
        "( " ^ 
        ArrayUtil.toString Int.toString a1 ^ ", " ^ 
        ArrayUtil.toString Int.toString a2 ^ 
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ArrayUtil", what="fill",
    genInput= fn() => [ ( Array.fromList[ 0, 1, 2, 3, 4, 5 ], ~1 ) ],
    f= fn[ i1 ] => [ ( ArrayUtil.fill i1; #1 i1 ) ],
    evaluate= 
      fn[ o1 ] =>
      let
        val t1 = Array.fromList[ ~1, ~1, ~1, ~1, ~1, ~1 ] 
      in 
        [ ArrayUtil.allEq Util.eqInt ( o1, t1 ) ]
      end ,
    inputToString= fn( a, _ ) => ArrayUtil.toString Int.toString a }
