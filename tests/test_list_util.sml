(* 
* file: test_list_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the functionality in the ListUtil 
* structure.
*)

val _ = print"\n\n************** ListUtil Tests **************\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ListUtil", what="Testing ListUtil.equal",
    genInput = 
      fn() => [ 
        ( [], [] ), 
        ( [ 1, 2 ], [ 1, 2 ] ),
        ( [ 3, 2 ], [ 2, 3 ] ),
        ( [ 3, 2 ], [ 3 ] ) ] ,
    f = 
      fn[ i1, i2, i3, i4 ] => [ 
        ListUtil.equal Util.eqInt i1,
        ListUtil.equal Util.eqInt i2,
        ListUtil.equal Util.eqInt i3,
        ListUtil.equal Util.eqInt i4 ] ,
    evaluate = fn[ o1, o2, o3, o4 ] => [ o1, o2, not o3, not o4 ] ,
    inputToString = fn( x, y ) => 
      "( " ^ 
      ListUtil.toString Int.toString x ^ ", " ^ 
      ListUtil.toString Int.toString y ^ 
      " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ListUtil", what="Testing ListUtil.fromToInt",
    genInput = fn() => [ ( ~5, 5 ) ] ,
    f = fn[ i1 ] => [ ListUtil.fromToInt i1 ] ,
    evaluate = 
      fn[ o1 ] => [
        ListUtil.equal Util.eqInt 
          ( o1, [ ~5, ~4, ~3, ~2, ~1, 0, 1, 2, 3, 4, 5 ] ) ] ,
    inputToString = fn( x, y ) => 
      "( " ^ 
      Int.toString x ^ ", " ^ 
      Int.toString y ^ 
      " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ListUtil", what="Testing ListUtil.fromToReal",
    genInput = fn() => [ ( ~1.5, 2.5 ) ] ,
    f = fn[ i1 ] => [ ListUtil.fromToReal i1 ] ,
    evaluate = 
      fn[ o1 ] =>
        [ ListUtil.equal Real.== ( o1, [ ~1.5, ~0.5, 0.5, 1.5, 2.5 ] ) ] ,
    inputToString = fn( x, y ) => 
      "( " ^ 
      Real.toString x ^ ", " ^ 
      Real.toString y ^ 
      " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ListUtil", what="Testing ListUtil.nestMap",
    genInput = fn() => [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] ] ,
    f = fn[ i1 ] => [ ListUtil.nestMap ( fn x=>x ) ( 3, i1 ) ] ,
    evaluate = 
      fn[ o1 : int list list ] =>
        [ ( ListUtil.equal
            ( ListUtil.equal ( fn ( x : int, y : int ) => x=y ) ) )
          ( o1, [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ] ) ] ,
    inputToString = fn( x : int list ) => "" 
(*      "( " ^ 
      Real.toString x ^ ", " ^ 
      Real.toString y ^ 
      " )"*) }
