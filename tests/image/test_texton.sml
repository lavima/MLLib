(* 
* file: test_texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests for validating texton generation
*)

val _ = print"\n\n********** Texton tests **********\n"

val _ = test( "Generate texton filters",
  fn() => Texton.createTextonFilters(8, 3.0),
  fn x =>
     let
        val _ = Util.loop (fn i =>
          let 
            val normalizedImage = ImageUtil.normalizeReal'' (List.nth (x, i) )
          in
            GrayscaleImageReal.save(
                    normalizedImage, "output/filter" ^ Int.toString(i))
          end
          ) (List.length x )
     in
         17 = List.length x
     end 
  )

val _ = test( "Generate textons",
  fn() => 
      let
          val image = Option.valOf(GrayscaleImageReal.load("test2.pgm"))
      in
         Texton.generateTextons(image, 8, 2.0, 32, 10)
      end,
  fn x : GrayscaleImageReal.image =>
     let
        val normalizedImage = ImageUtil.normalizeReal'' x
        
        val _ =GrayscaleImageReal.save(normalizedImage, "output/textons.pgm")
     in
         true
     end 
  )
  ;
