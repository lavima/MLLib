(* 
* file: test_texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests for validating texton generation
*)

val _ = print"\n\n********** Multiscale cue tests **********\n"

val _ = test( "Generate oriented multiscale cue response",
  fn() => 
    let
      val savgol_A = [(3.0, 3.0 / 4.0), (5.0, 5.0/4.0), (10.0, 10.0 / 4.0)]
      val img = Option.valOf(GrayscaleImageReal.load("proper.plain.pgm"))
      val weights = [1.0, 1.0, 1.0]
      val grad = MultiscaleCue.orientedMultiscale
         (img, weights, 32, 10, Math.pi / 4.0, savgol_A)
    in
      grad
    end,
  fn x =>
     let
         val normalizedImage = ImageUtil.normalizeReal'' x
         val _ = GrayscaleImageReal.save
            (normalizedImage, "output/OrientedMultiscale.pgm")
     in
         true
     end 
  )

val _ = test( "Generate multiscale cue response",
  fn() => 
    let
      val img = Option.valOf(GrayscaleImageReal.load("proper.plain.pgm"))
      val weights = [1.0, 1.0, 1.0]
      val grad = MultiscaleCue.multiscale
         (img, img, weights, 32, 10, 8)
    in
      grad
    end,
  fn x =>
     let
         val normalizedImage = ImageUtil.normalizeReal'' x
         val _ = GrayscaleImageReal.save
            (normalizedImage, "output/Multiscale.pgm")
     in
         true
     end 
  )

