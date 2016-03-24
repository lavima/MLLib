(*
* filename: test_filter.sml
* author: marius.geitle <marius.geitle@hiof.no
*
* Code to test the functionality in the filter.sml file
*)



val _ = print"\n\n********** Filter tests **********\n"

val _ = UnitTest.test( "Convolving with zero border extension",
  fn() => 
     let
        val mask1 = GrayscaleImageReal.fromList(1, 5, [1.0, 2.0, 3.0, 2.0, 1.0])
        val mask2 = GrayscaleImageReal.fromList(5, 1, [1.0, 2.0, 3.0, 2.0, 1.0])
        val output = GrayscaleImageReal.convolve
             (ImageCommon.zero, ImageCommon.full)
             (mask2, mask1)
        val _ = print (GrayscaleImageReal.toString output);
     in 
        output
     end,
  fn X => true
  )
  ;
