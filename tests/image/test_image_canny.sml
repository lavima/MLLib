(*
* file: test_image_canny.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the Canny structure in the image 
* library.
*)

val _ = print"\n\n********** Image Canny Tests **********\n"

val _ = 
  UnitTest.test( "Testing Canny on a simple generated grayscale image",
    fn() => 
      let
        val list = 
          [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0,
            0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ]
        val im = RealGrayscaleImage.fromList'( 8, 8, list )
        val edges = Canny.findEdges im
      in
        edges
      end ,
    fn edges => 
      true )

val _ = 
  UnitTest.test( "Testing Canny on proper grayscale image",
    fn() => 
      let
        val im = Option.valOf( RealPGM.read("proper2.raw.pgm") )
        val edges = 
          Canny.findEdges' ( Math.sqrt 2.0, Canny.highLow( 0.5, 0.3 ) ) im
        val _ = BooleanPBM.write( edges, "output/canny.pbm" )
      in
        Option.valOf( BooleanPBM.read("output/canny.pbm") )
      end ,
    fn edges => 
    let
    in
      true
    end )

(*
val _ = 
  test( "Testing Canny with Otsu thresholding on proper grayscale image",
    fn() => 
      let
        val Image = Option.valOf( GrayscaleImageReal.load("proper.plain.pgm") )
        val Edges = Canny.findEdges' ( Canny.otsuHighLowRatio 0.8 ) Image
        val _ = BooleanImage.save( Edges, "canny_otsu.pbm" )
      in
        Option.valOf( BooleanImage.load("canny.pbm") )
      end ,
    fn X => 
    let
    in
      true
    end )
*)
