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
    val Image = Option.valOf( GrayscaleImageReal.load("simple.plain.pgm") )
  in
    GrayscaleImageReal.thresholds' ( ImageCommon.percentage( 4, 0.5 ) ) Image
  end,
  fn Xs =>
    let
      val [ X ] = Xs
    in
      Util.eqReal( X, 0.75 )
    end )

val _ = test( "Testing otsu thresholding on a simple image",
  fn() => 
  let
    val Image = Option.valOf( GrayscaleImageReal.load("simple.plain.pgm") )
  in
    GrayscaleImageReal.thresholds' ( ImageCommon.otsu 256 ) Image 
  end,
  fn Xs => 
  let
    val [ X ] = Xs
  in
    Util.approxEqReal( X, 0.749019607843137, 3 )
  end )

val _ = test( "Testing otsu thresholding on a proper image",
  fn() => 
  let
    val Image = Option.valOf( GrayscaleImageReal.load("proper.raw.pgm") )
  in
    GrayscaleImageReal.thresholds' ( ImageCommon.otsu 256 ) Image
  end,
  fn Xs => 
  let
    val [ X ] = Xs
  in
    Util.approxEqReal( X, 0.439215686274510, 3 )
  end )
