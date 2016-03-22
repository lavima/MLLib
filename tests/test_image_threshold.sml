(* 
* file: test_image_threshold.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the Threshold structure in the image 
* library.
*)

val _ = print"\n\n********** Image Threshold Tests **********\n"

val _ = test( "Testing percentage thresholding on a simple image",
  fn() => 
  let
    val im = Option.valOf( RealPGM.read("simple.plain.pgm") )
  in
    RealGrayscaleThreshold.percentage( im, 4, 0.5 )
  end,
  fn t => Real.==( t, 0.75 ) )

val _ = test( "Testing otsu thresholding on a simple image",
  fn() => 
  let
    val im = Option.valOf( RealPGM.read("simple.plain.pgm") )
  in
    RealGrayscaleThreshold.otsu( im, 256 ) 
  end,
  fn t => Util.approxEqReal( t, 0.749019607843137, 3 ) )

val _ = test( "Testing otsu thresholding on a proper image",
  fn() => 
  let
    val im = Option.valOf( RealPGM.read("proper.raw.pgm") )
  in
    RealGrayscaleThreshold.otsu( im, 256 )
  end,
  fn t => Util.approxEqReal( t, 0.439215686274510, 3 ) )
