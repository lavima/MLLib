(* 
* file: test_texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests for validating texton generation
*)

val _ = print"\n\n********** Multiscale cue tests **********\n"

val _ = UnitTest.test( "Generate oriented multiscale cue response",
  fn() => 
    let
      val savgol_A = [ ( 3.0, 3.0/4.0 ), ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ) ]
      val img = Option.valOf( RealPGM.read "proper.plain.pgm" )
      val weights = [ 1.0, 1.0, 1.0 ]
      val grad = MultiscaleCue.orientedMultiscale
         ( img, weights, 32, 10, Math.pi/4.0, savgol_A )
    in
      grad
    end,
  fn x =>
    let
      val normalizedImage = ImageUtil.normalizeReal'' x
      val _ = RealPGM.write( normalizedImage, "output/OrientedMultiscale.pgm" )
    in
      true
    end )

val _ = UnitTest.test( "Generate multiscale cue response",
  fn() => 
    let
      val configuration = {
        channelL = {
          weights = [ 0.0146, 0.0145, 0.0163 ],
          savgolFilters = 
            [ ( 3.0, 3.0/4.0), ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ) ],
          bins = 25,
          scale = 5,
          nori = 8
        },
        channelA = {
          weights = [ 0.0210, 0.0243, 0.0287 ],
          savgolFilters = 
            [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
          bins = 25,
          scale = 10,
          nori = 8
        },
        channelB = {
          weights = [ 0.0166, 0.0185, 0.0204 ],
          savgolFilters = 
            [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
          bins = 25,
          scale = 10,
          nori = 8
        },
        channelT = {
          weights = [ 0.0101, 0.0111, 0.0141 ],
          savgolFilters = 
            [ ( 5.0, 5.0/4.0 ), ( 10.0, 10.0/4.0 ), ( 20.0, 20.0/4.0 ) ],
          bins = 25,
          scale = 10,
          nori = 8
        },
        texton = {
          nori = 8,
          sigma = 2.0,
          nTextons = 32,
          maxIterations = 200
        },
        border = 30
      }

      val img = Option.valOf( RealPPM.read "proper2.raw.ppm" )
      val grad = MultiscaleCue.multiscale configuration img
    in
      grad
    end,
  fn x =>
    let
      val normalizedImage = ImageUtil.normalizeReal'' x
      val _ = RealPGM.write( normalizedImage, "output/Multiscale.pgm" )
    in
      true
    end 
  )

