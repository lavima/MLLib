(* 
* file: test_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the functionality in the Util 
* structure.
*)


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.eq with reals",
    genInput = fn() => [ ( 3.0, 10.0 ), ( 102.0, 102.0 ), ( 32.0, 9.0 ) ] ,
    f = 
      fn[ i1, i2, i3 ] => [
        Util.eq Real.compare i1,
        Util.eq Real.compare i2,
        Util.eq Real.compare i3 ] ,
    evaluate = fn[ x, y, z ] => [ not x, y, not z ] ,
    inputToString = fn( x, y ) => 
      "( " ^ 
      Real.toString x ^ ", " ^ 
      Real.toString y ^ 
      " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.approxEqReal",
    genInput = fn() => [ ( 1.001, 1.002, 2 ), ( 1.001, 1.002, 3 ) ] ,
    f = fn [ i1, i2 ]  => [ Util.approxEqReal i1, Util.approxEqReal i2 ] ,
    evaluate = fn[ x, y ] => [ x, not y ] ,
    inputToString = fn( x, y, n ) => 
      "( " ^ 
      Real.toString x ^ ", " ^ 
      Real.toString y ^ ", " ^ 
      Int.toString n ^ 
      " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.approxEqReal'",
    genInput = 
      fn() => [
        ( 1.124546548778789,  1.124546548778788, 15 ),
        ( 1.124546548778789,  1.124546548778788, 16 ),
        ( 1.124546548778789,  1.124546548778789, 16 ) ] ,
    f = 
      fn[ i1, i2, i3 ] => [ 
        Util.approxEqReal' i1,
        Util.approxEqReal' i2,
        Util.approxEqReal' i3 ] ,
    evaluate = fn[ x, y, z ] => [ x, not y, z ] ,
    inputToString = fn( x, y, n ) => 
      "( " ^ 
      Real.toString x ^ ", " ^ 
      Real.toString y ^ ", " ^ 
      Int.toString n ^ 
      " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.eqInt",
    genInput = fn() => [ ( 3, 10 ), ( 102, 102 ), ( 32, 9 ) ] ,
    f =
      fn[ i1, i2, i3 ] => [ Util.eqInt i1, Util.eqInt i2, Util.eqInt i3 ] ,
    evaluate = fn[ x, y, z ] => [ not x, y, not x ] ,
    inputToString = fn( x, y ) => 
      "( " ^ 
      Int.toString x ^ ", " ^ 
      Int.toString y ^ 
      " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.loop",
    genInput = fn() => [ 10, 100, 10000 ] ,
    f =
      fn[ i1, i2, i3 ] =>
      let
        val indices1 = Array.array( i1, 0 )
        val indices2 = Array.array( i2, 0 )
        val indices3 = Array.array( i3, 0 )
        val _ = Util.loop ( fn i => Array.update( indices1, i, i ) ) i1
        val _ = Util.loop ( fn i => Array.update( indices2, i, i ) ) i2
        val _ = Util.loop ( fn i => Array.update( indices3, i, i ) ) i3
      in
        [ indices1, indices2, indices3 ]
      end ,
    evaluate =
      fn[ o1, o2, o3 ] => [
        Array.foldli ( fn( i, i', t ) => t andalso i=i' ) true o1,
        Array.foldli ( fn( i, i', t ) => t andalso i=i' ) true o2,
        Array.foldli ( fn( i, i', t ) => t andalso i=i' ) true o3 ] ,
    inputToString = fn x => Int.toString x }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.max with reals",
    genInput = 
      fn() => [ 
        [ ~1.0, 2.0, 3.0, ~10.0 ], 
        [ 100.0, 10.0, 1.0 ], 
        [ ~1.0, ~2.0, ~1.5 ] ] ,
    f = 
      fn[ i1, i2, i3 ] => [
        Util.max Real.< i1,
        Util.max Real.< i2,
        Util.max Real.< i3 ] ,
    evaluate = 
      fn[ o1, o2, o3 ] => 
        [ Real.==( o1, 3.0 ), Real.==( o2, 100.0 ), Real.==( o3, ~1.0 ) ] ,
    inputToString = ListUtil.toString Real.toString }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.maxInt",
    genInput = 
      fn() => [
        [ ~1, 2, 3, ~10 ], [ 100, 10, 1 ], [ ~1, ~2, ~3 ] ] ,
    f = fn[ i1, i2, i3 ] => [ Util.maxInt i1, Util.maxInt i2, Util.maxInt i3 ] ,
    evaluate = fn[ o1, o2, o3 ] => [ o1=3, o2=100, o3= ~1 ] ,
    inputToString = ListUtil.toString Int.toString }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.minInt",
    genInput = 
      fn() => [
        [ ~1, 2, 3, ~10 ], [ 100, 10, 1 ], [ ~1, ~2, ~3 ] ] ,
    f = fn[ i1, i2, i3 ] => [ Util.minInt i1, Util.minInt i2, Util.minInt i3 ] ,
    evaluate = fn[ o1, o2, o3 ] => [ o1= ~10, o2=1, o3= ~3 ] ,
    inputToString = ListUtil.toString Int.toString }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.max2 with reals",
    genInput = 
      fn() => [ ( 3.0, ~4.0 ), ( ~3.0, 4.0 ), ( 3.0, 4.0 ), ( ~3.0, ~4.0 ) ] ,
    f = 
      fn[ i1, i2, i3, i4 ] => [ 
        Util.max2 Real.< i1,
        Util.max2 Real.< i2,
        Util.max2 Real.< i3,
        Util.max2 Real.< i4 ] ,
    evaluate = 
      fn[ o1, o2, o3, o4 ] => [ 
        Real.==( o1, 3.0 ), 
        Real.==( o2, 4.0 ), 
        Real.==( o3, 4.0 ),
        Real.==( o4, ~3.0 ) ] ,
    inputToString = fn( x, y ) => 
      "( " ^ 
      Real.toString x ^ ", " ^ 
      Real.toString y ^ 
      " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.max2Int",
    genInput = fn() => [ ( 3, ~4 ), ( ~3, 4 ), ( 3, 4 ), ( ~3, ~4 ) ] ,
    f = 
      fn[ i1, i2, i3, i4 ] => 
        [ Util.max2Int i1, Util.max2Int i2, Util.max2Int i3, Util.max2Int i4 ] ,
    evaluate = fn[ o1, o2, o3, o4 ] => [ o1=3, o2=4, o3=4, o4= ~3 ] ,
    inputToString = fn( x, y ) => 
      "( " ^ 
      Int.toString x ^ ", " ^ 
      Int.toString y ^ 
      " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) { 
    group="Util", what="Testing Util.min2Int",
    genInput = fn() => [ ( 3, ~4 ), ( ~3, 4 ), ( 3, 4 ), ( ~3, ~4 ) ] ,
    f = 
      fn[ i1, i2, i3, i4 ] => 
        [ Util.min2Int i1, Util.min2Int i2, Util.min2Int i3, Util.min2Int i4 ] ,
    evaluate = fn[ o1, o2, o3, o4 ] => [ o1= ~4, o2= ~3, o3=3, o4= ~4 ] ,
    inputToString = fn( x, y ) => 
      "( " ^ 
      Int.toString x ^ ", " ^ 
      Int.toString y ^ 
      " )" }
