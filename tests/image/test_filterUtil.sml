(* 
* file: test_filterUtil.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the filter utilities
*)

val _ = print"\n\n********** Filter utility tests **********\n"

val _ = test( "Generating 1D gaussian filter created using the gPb type",
  fn() => FilterUtil.createGaussianMaskgPb 0 (3.0, 9),
  fn X =>
     let
         val _ = GrayscaleImageReal.save(X, "output/filterThingy.pbm");
     in
         true
     end 
  )
  ;


val _ = test( "Generating 2D gaussian filter created using the gPb type",
  fn() => FilterUtil.createGaussianMaskGPB2D 
                  2 (3.0, 3.0, 3.0, 1.0, false, 0.3),
  fn X =>
     let
        val norm = ImageUtil.normalizeReal'' X 
        val _ = GrayscaleImageReal.save(norm, "output/output3dFilter.pbm");
     in
         true
     end 
  )
  ;

val _ = test( "Savgol filtering",
  fn() => 
    let
      val img = Option.valOf(GrayscaleImageReal.load("proper.plain.pgm"))
    in
      FilterUtil.savgol(img, 3.0, 1.0, 0.6)
    end,
  fn x =>
     let
         val normalizedImage = ImageUtil.normalizeReal'' x
         val _ = GrayscaleImageReal.save
            (normalizedImage, "output/savgolFiltering.pgm")
     in
         true
     end 
  )

