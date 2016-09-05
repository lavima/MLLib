(*
* file: test_adate_canny.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the ADATECanny structure in the image 
* library.
*)

val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ADATECanny", what="findEdges'",
    genInput=
      fn() =>
        [ ( [ ADATECanny.filterMask ],
            ( 3.25, Canny.highLow( 0.25, 0.125 ) ), 
            Option.valOf( RealPGM.read"resources/proper3.raw.pgm" ) ),
          ( [ ADATECanny.nonMaxSuppression ],
            ( 3.25, Canny.highLow( 0.25, 0.125 ) ), 
            Option.valOf( RealPGM.read"resources/proper3.raw.pgm" ) ),
          ( [ ADATECanny.hysteresisThresholding ],
            ( 3.25, Canny.highLow( 0.25, 0.125 ) ), 
            Option.valOf( RealPGM.read"resources/proper3.raw.pgm" ) )] ,
    f=
      fn[ i1, i2, i3 ] =>
        [ ADATECanny.findEdges' ( #1 i1 ) ( #2 i1 ) ( #3 i1 ),
          ADATECanny.findEdges' ( #1 i2 ) ( #2 i2 ) ( #3 i2 ),
          ADATECanny.findEdges' ( #1 i3 ) ( #2 i3 ) ( #3 i3 )],
    evaluate=
      fn[ o1, o2, o3 ] =>
      let
        val t1 = 
          Option.valOf( 
            BooleanPBM.read( "resources/proper3.canny_mask.truth.pbm" ) )
        val t2 = 
          Option.valOf( 
            BooleanPBM.read( "resources/proper3.canny_nonmax.truth.pbm" ) )
        val t3 = 
          Option.valOf( 
            BooleanPBM.read( "resources/proper3.canny_hysteresis.truth.pbm" ) )
      in
        [ BooleanImage.equal( o1, t1 ),
          BooleanImage.equal( o2, t2 ),
          BooleanImage.equal( o3, t3 ) ]
      end ,
    inputToString=
      fn( improvements, options, i ) => RealGrayscaleImage.toString i }
