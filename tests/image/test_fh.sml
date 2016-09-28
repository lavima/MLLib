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
        ( Option.valOf( RealPGM.read"resources/simple.segment.raw.pgm" ), 
          1.0, 1.0, 1.0 ) ] ,
    f= fn[ i1, i2 ] => [ FH.segment i1, FH.segment i2 ] ,
    evaluate= 
      fn[ o1, o2 ] =>
      let
        val _ = 
          IntPGM.write' 
            ( PNM.plainPGM, 0w65535 ) 
            ( o1, "output/proper3.fh.output.pgm" ) 
        val _ = 
          IntPGM.write' 
            ( PNM.plainPGM, 0w255 ) 
            ( o2, "output/simple.segment.fh.output.pgm" ) 
        val t1 = Option.valOf( IntPGM.read"resources/proper3.fh.truth.pgm" )
        val t2 = Option.valOf( IntPGM.read"resources/simple.segment.fh.truth.pgm" )

        val score1 = 
          FMeasureBerkeley.evaluateSegmentation( o1, [ Segment.toEdgeMap t1 ] )
        val score2 = 
          FMeasureBerkeley.evaluateSegmentation( o2, [ Segment.toEdgeMap t2 ] )

        val _ = print( FMeasureBerkeley.toString score1 ^ "\n" )
        val _ = print( FMeasureBerkeley.toString score2 ^ "\n" )
      in
        [ true ]
      end ,
    inputToString= fn( im, _, _, _ ) => RealGrayscaleImage.toString im }

