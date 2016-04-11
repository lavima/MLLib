(*
* file: test_canny.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the Canny structure in the image 
* library.
*)

val _ = print"\n\n********** Canny Tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Canny", what="findEdges",
    genInput= 
      fn() =>
        [ ( ( Math.sqrt 2.0, Canny.highLow( 0.5, 0.3 ) ),
            RealGrayscaleImage.fromList[
              [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ],
              [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ],
              [ 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0 ],
              [ 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0 ],
              [ 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0 ],
              [ 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0 ],
              [ 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0 ],
              [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] ] ),
          ( ( Math.sqrt 2.0, Canny.highLow( 0.5, 0.3 ) ), 
            Option.valOf( RealPGM.read("proper2.raw.pgm") ) ) ] , 
    f= 
      fn[ i1, i2 ] => 
        [ Canny.findEdges' ( #1 i1 ) ( #2 i1 ), 
          Canny.findEdges' ( #1 i2 ) ( #2 i2 ) ] ,
    evaluate= fn[ o1, o2 ] => [ true, true ] ,
    inputToString=  
      fn( options, i ) =>
        RealGrayscaleImage.toString i }
