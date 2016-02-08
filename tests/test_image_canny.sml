(*
* file: test_image_canny.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the Canny structure in the image 
* library.
*)

val _ = print"\n\n********** Image Canny Tests **********\n"

val _ = 
  test( "Testing Canny on a simple generated grayscale image",
    fn() => 
      let
        val List = 
          [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0,
            0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ]
        val Image = GrayscaleImageReal.fromList( 8, 8, List )
        val Edges = Canny.findEdges Image
      in
        Edges
      end ,
    fn X => 
      true )

val _ = 
  test( "Testing Canny on proper grayscale image",
    fn() => 
      let
        val Image = Option.valOf( GrayscaleImageReal.load("proper2.raw.pgm") )
        val Edges = 
          Canny.findEdges' ( Math.sqrt 2.0, Canny.highLow( 0.5, 0.3 ) ) Image
        val _ = BooleanImage.save( Edges, "canny.pbm" )
      in
        Option.valOf( BooleanImage.load("canny.pbm") )
      end ,
    fn X => 
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
