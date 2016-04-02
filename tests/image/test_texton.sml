(* 
* file: test_texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests for validating texton generation
*)

val _ = print"\n\n********** Texton tests **********\n"

val _ = UnitTest.test( "Generate texton filters",
  fn() => Texton.createTextonFilters(8, 3.0),
  fn x =>
     let
        val _ = Util.loop (fn i =>
          let 
            val normalizedImage = ImageUtil.normalizeReal'' (List.nth (x, i) )
          in
            RealPGM.write(
              normalizedImage, 
              "output/filter" ^ Int.toString(i) ^ ".pgm" )
          end
          ) (List.length x )
     in
         17 = List.length x
     end 
  )

val _ = UnitTest.test( "Generate textons",
  fn() => 
      let
          val image = Option.valOf(RealPGM.read("test2.pgm"))
      in
         Texton.generateTextons(image, 8, 2.0, 32, 10)
      end,
  fn x : RealGrayscaleImage.image =>
     let
        val normalizedImage = ImageUtil.normalizeReal'' x
        
        val _ = RealPGM.write(normalizedImage, "output/textons.pgm")
     in
         true
     end 
  )
