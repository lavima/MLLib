(*
* file: test_fh.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the FH structure in the image 
* library.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="FH", what="segment",
    genInput=
      fn() => [ 
        ( Option.valOf( RealPGM.read"resources/proper3.raw.pgm" ), 
          2.5, 3.0, 20.0 ) ] ,
    f= fn[ i1 ] => [ FH.segment i1 ] ,
    evaluate= 
      fn[ o1 ] =>
      let
        val _ = IntPGM.write( o1, "output/proper3.fh.output.pgm" ) 
      in
        [ true ]
      end ,
    inputToString= fn( im, _, _, _ ) => RealGrayscaleImage.toString im }

