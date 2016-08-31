(*
* file: test_adate_canny.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the ADATECanny structure in the image 
* library.
*)

val _ = print"\n\n********** ADATECanny Tests **********\n"

val _ =
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ADATECanny", what="findEdges'",
    genInput=
      fn() =>
        [ ( ( 3.25, Canny.highLow( 0.25, 0.125 ) ), 
            Option.valOf( RealPGM.read"resources/proper3.raw.pgm" ) ) ] ,
    f=
      fn[ i1 ] =>
        [ ADATECanny.findEdges' [ ADATECanny.filterMask ] ( #1 i1 ) ( #2 i1 ) ],
    evaluate=
      fn[ o1 ] =>
      let

        val _ = BooleanPBM.write( o1, "output/proper3.canny_mask.output.pbm" )
        val t1 = 
          Option.valOf( 
            BooleanPBM.read( "resources/proper3.canny_mask.truth.pbm" ) )

      in
        [ BooleanImage.equal( o1, t1 ) ]
      end ,
    inputToString=
      fn( options, i ) => RealGrayscaleImage.toString i }
