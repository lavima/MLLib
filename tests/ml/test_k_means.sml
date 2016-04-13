(* 
* file: test_k_means.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the functionality in the KMeans 
* structure.
*)

val _ = print"\n\n********** Testing KMeans **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="KMeans", what="cluster",
    genInput=
      fn() => 
        [ ( 2, 
            1, 
            [ [ 0.1 ], [ 0.2 ], [ 0.3 ], [ 0.6 ], [ 0.7 ], [ 0.8 ] ], 
            100 ),
          ( 2, 
            2,
            [ [ 0.1, 0.9 ], 
              [ 0.2, 0.2 ], 
              [ 0.3, 0.1 ], 
              [ 0.6, 0.6 ], 
              [ 0.7, 0.2 ], 
              [ 0.8, 0.7 ] ],
            100 ) ] ,
    f= fn[ i1, i2 ] => [ KMeans.cluster i1, KMeans.cluster i2 ] ,
    evaluate= fn[ o1, o2 ] => [ true, true ] ,
    inputToString= 
      fn( k, n, is, m ) =>
        "( " ^
        Int.toString k ^ ", " ^
        Int.toString n ^ ", " ^
        ListUtil.toString ( ListUtil.toString Real.toString ) is ^ ", " ^
        Int.toString m ^ 
        " )" }
