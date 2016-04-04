(* 
* file: test_filter_util.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the image utility functionality.
*)

val _ = print"\n\n********** Image utility tests **********\n"

val _ = UnitTest.test( "Approximate real grayscale image comparison",
  fn() => 
  let
    val img = RealGrayscaleImage.fromList( 
                [ [ 0.11, 0.21, 0.35 ],
                  [ 0.42, 0.53, 0.62 ],
                  [ 0.73, 0.85, 0.91 ] ] )
  in
    img
  end,
  fn x =>
  let
    val expectedTrue = RealGrayscaleImage.fromList( 
                [ [ 0.11, 0.21, 0.35 ],
                  [ 0.42, 0.53, 0.62 ],
                  [ 0.73, 0.85, 0.91 ] ] )

    val expectedFalse = RealGrayscaleImage.fromList( 
                [ [ 0.10, 0.21, 0.35 ],
                  [ 0.41, 0.53, 0.62 ],
                  [ 0.73, 0.82, 0.91 ] ] )

  in
    ImageUtil.approxCompareGrayscaleReal( x, expectedTrue, 2 ) andalso 
    not ( ImageUtil.approxCompareGrayscaleReal( x, expectedFalse, 2 ) )
  end )
