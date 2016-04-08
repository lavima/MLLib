(* 
* file: test_pbm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the PBM implementations.
*)

val _ = print"\n\n********** PBM Tests **********\n"

val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="BooleanPBM", what="read",
    genInput= fn() => [ "simple.plain.pbm", "simple.raw.pbm" ] ,
    f= 
      fn[ i1, i2 ] => [
        Option.valOf( BooleanPBM.read i1 ) ,
        Option.valOf( BooleanPBM.read i2 ) ] ,
    evaluate=
      fn[ o1, o2 ] =>
      let
        val truth = BooleanImage.fromList[ [ true, false ], [ true, false ] ]
      in [
        BooleanImage.equals( o1, truth ),
        BooleanImage.equals( o2, truth ) ]
      end ,
    inputToString= fn x => x }

