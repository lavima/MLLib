(* 
* file: test_morphology.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the Morphology structure.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Morphology", what="thin",
    genInput=
      fn() => 
      let
        val t = true
        val f = false
      in
        [ BooleanImage.fromList[
            [ t, t, t, t, t, t, t, t, t, t, t ], 
            [ t, t, t, t, t, t, t, t, t, f, f ],
            [ t, t, t, t, t, t, t, t, t, f, f ],
            [ t, t, t, t, t, t, t, t, t, f, f ],
            [ t, t, t, f, f, t, t, t, t, f, f ] ] ]
      end ,
    f= fn[ i1 ] => [ Morphology.thin i1 ] , 
    evaluate=
      fn[ o1 ] =>
      let 
        val t = true
        val f = false
        val truth = 
          BooleanImage.fromList[ 
            [ t, f, f, f, f, f, f, f, t, t, t ], 
            [ t, t, f, f, f, f, f, t, t, f, f ], 
            [ f, t, t, t, t, t, t, t, f, f, f ],
            [ t, t, f, f, f, f, f, t, f, f, f ],
            [ t, f, f, f, f, f, f, t, t, f, f ] ]
      in
        [ BooleanImage.equal( o1, truth ) ]
      end ,
    inputToString= BooleanImage.toString }
