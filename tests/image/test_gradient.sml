(* 
* file: test_gradient.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the gradient
*)

val _ = print"\n\n********** gradient tests **********\n"

val _ = UnitTest.test( "Gradient",
  fn() => 
     let
        val img = Option.valOf(RealPGM.read("proper.plain.pgm"))
        val grad = Gradient.gradient(img, 32, 7, 0.1)
     in
        grad
     end,
  fn x =>
     let
         val normalizedImage = ImageUtil.normalizeReal'' x
         val _ = RealPGM.write(normalizedImage, "output/gradient.pgm")
     in
         true
     end 
  )

val _ = UnitTest.test( "Oriented gradient",
  fn() => 
  let
    val img = Option.valOf(RealPGM.read("proper.plain.pgm"))
    val grad = Gradient.orientedGradientReal(img, 32, 20, 0.6, ( 5.0, 5.0/4.0 ), 0.1)
  in
    grad
  end ,
  fn x =>
  let
    val normalizedImage = ImageUtil.normalizeReal'' x
    val _ = RealPGM.write(normalizedImage, "output/Orientedgradient.pgm")
  in
    true
  end )
