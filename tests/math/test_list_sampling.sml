(* 
* file: test_list_sampling.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the list sampling functionality
*)


val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ListSampling", what="Sample k items",
    genInput=
      fn() => 
        [ ( Random.rand( 1, 1 ), [ 3, 4, 66, 8, 5, 2, 1, 4, 6, 4, 2 ], 5 ) ] ,
    f= fn[ i1 ] => [ ListSampling.sampleK i1 ] ,
    evaluate= fn[ o1 ] => [ List.length o1=5 ] ,
    inputToString=
      fn( _, xs, k ) =>
        "( " ^ 
        ListUtil.toString Int.toString xs ^ ", " ^ 
        Int.toString k ^
        " )" }
