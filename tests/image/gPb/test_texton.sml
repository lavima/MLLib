(* 
* file: test_texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests for validating texton generation
*)

val _ = print"\n\n********** Texton tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Texton", what="createTextonFilters",
    genInput= fn() => [ ( 8, 3.0 ) ] ,
    f= fn[ i1 ] => [ Texton.createTextonFilters i1 ],
    evaluate= fn[ o1 ] => [ List.length o1=17 ] ,
    inputToString= 
      fn( n, s ) =>
        "( " ^
        Int.toString n ^ ", " ^
        Real.toString s ^ 
        " )" }

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Texton", what="generateTextons",
    genInput= 
      fn() => 
        [ ( Option.valOf( RealPGM.read "test2.pgm" ), 8, [ 2.0 ], 32, 10 ) ] ,
    f= fn[ i1 ] => [ Texton.generateTextons i1 ] ,
    evaluate= 
      fn[ o1 ] => 
      let
        val _ = IntPGM.write( o1, "output/textons.pgm" )
      in
        [ true ]
      end,
    inputToString=
      fn( im, n, s, k, m ) =>
        "( " ^
        RealGrayscaleImage.toString im ^ ", " ^
        Int.toString n ^ ", " ^
        ListUtil.toString Real.toString s ^ ", " ^
        Int.toString k ^ ", " ^
        Int.toString m ^
        " )" }
