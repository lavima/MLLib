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
          1.0, 2.0, 20.0 ),
        ( Option.valOf( RealPGM.read"resources/proper3.sub.raw.pgm" ), 
          1.0, 2.0, 20.0 ) ] ,
    f= fn[ i1, i2 ] => [ FH.segment i1, FH.segment i2 ] ,
    evaluate= 
      fn[ o1, o2 ] =>
      let
        val _ = IntPGM.write( o1, "output/proper3.fh.output.pgm" ) 
        val _ = IntPGM.write( o2, "output/proper3.sub.fh.output.pgm" ) 
        val t1 = Option.valOf( IntPGM.read"resources/proper3.fh.truth.pgm" )

        val score = 
          FMeasureBerkeley.evaluateSegmentation( o1, [ Segment.toEdgeMap t1 ] )

        val _ = print( FMeasureBerkeley.toString score ^ "\n" )
      in
        [ true ]
      end ,
    inputToString= fn( im, _, _, _ ) => RealGrayscaleImage.toString im }

