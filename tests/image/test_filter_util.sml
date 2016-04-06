(* 
* file: test_filter_util.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the filter utilities
*)

val _ = print"\n\n********** Filter utility tests **********\n"

val _ = UnitTest.test( "Generating 1D gaussian filter created using the gPb type",
  fn() => FilterUtil.createGaussianMaskgPb 0 (3.0, 9) ,
  fn X =>
  let
    val _ = RealPGM.write(X, "output/filterThingy.pgm")
  in
    true
  end )


val _ = UnitTest.test( "Generating 2D gaussian filter created using the gPb type",
  fn() => 
    FilterUtil.createGaussianMaskGPB2D 0 (3.0, 9.0, 9.0, 3.0, false, 0.3),
  fn X =>
  let
    val norm = ImageUtil.normalizeReal'' X 
    val _ = RealPGM.write(norm, "output/output3dFilter.pgm");
  in
    true
  end )

val _ = UnitTest.test( "Savgol filtering",
  fn() => 
  let
    val img = Option.valOf(RealPGM.read("proper.plain.pgm"))
  in
    FilterUtil.savgol(img, 3.0, 1.0, 0.6)
  end,
  fn x =>
  let
    val normalizedImage = ImageUtil.normalizeReal'' x
    val _ = RealPGM.write( normalizedImage, "output/savgolFiltering.pgm" )
  in
    true
  end )


val _ = UnitTest.test( "applying gamma correction",
  fn() => 
  let
    val img = RealGrayscaleImage.fromList( 
                [ [ 0.1, 0.2, 0.3 ],
                  [ 0.4, 0.5, 0.6 ],
                  [ 0.7, 0.8, 0.9 ] ] )
    val _ = FilterUtil.applyGammaCorrection( img, 2.5 )
  in
    img
  end,
  fn x =>
  let
    val expected = RealGrayscaleImage.fromList(
                     [ [ 0.003162278, 0.017888544, 0.04929503 ],
                       [ 0.101192885, 0.176776695, 0.278854801 ],
                       [ 0.409963413, 0.572433402, 0.768433471 ] ] )
  in
    ImageUtil.approxCompareGrayscaleReal( x, expected, 5 )
  end )
