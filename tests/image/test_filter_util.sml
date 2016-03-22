(* 
* file: test_filter_util.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the filter utilities
*)

val _ = print"\n\n********** Filter utility tests **********\n"

val _ = test( "Generating 1D gaussian filter created using the gPb type",
  fn() => FilterUtil.createGaussianMaskgPb 0 (3.0, 9),
  fn X =>
     let
         val _ = RealPGM.write(X, "output/filterThingy.pgm");
     in
         true
     end 
  )


val _ = test( "Generating 2D gaussian filter created using the gPb type",
  fn() => FilterUtil.createGaussianMaskGPB2D 
                  2 (3.0, 3.0, 3.0, 1.0, false, 0.3),
  fn X =>
     let
        val norm = ImageUtil.normalizeReal'' X 
        val _ = RealPGM.write(norm, "output/output3dFilter.pgm");
     in
         true
     end 
  )
