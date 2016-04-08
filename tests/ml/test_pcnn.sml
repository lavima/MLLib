(*
* file: test_pcnn.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file validates the PCNN functionality.
*)

val _ = print "\n\n********** Testing PCNN **********\n"

val _ = UnitTest.test( "Testing PCNN.fastLinkingIterate",
  fn() => 
  let
    val img = Option.valOf( RealPGM.read "12003.pgm" )
    val ( height, width ) = RealGrayscaleImage.dimensions img
    val pcnn = PCNN.create(height, width, 0.0, 0.367879, 0.000045, 0.5, 0.0, 1.0, 2.0, fn x => 1.0/(x*x) )
    val _ = PCNN.fastLinkingIterate(pcnn, 1.5, 0, img)
    val _ = PCNN.fastLinkingIterate(pcnn, 1.5, 16, img)
    val y = #y pcnn
  in
    y
  end,
  fn x => 
  let
    val _ = BooleanPBM.write(x, "output/pcnn.pbm" )
  in
    true
  end )
