(*
* file: test_connected-component.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests connected components functionality.
* 
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="ConnectedComponents", what="labelComponents",
    genInput= 
      fn() =>
      let
        val t = true
        val f = false
      in
        [ BooleanImage.fromList[ 
            [ t, t, t, f, f, f, f, f, f, f, f ], 
            [ t, t, t, f, f, f, f, f, f, f, f ],
            [ t, t, t, f, f, t, t, t, f, f, f ],
            [ f, f, f, f, f, t, t, t, f, f, f ],
            [ f, f, f, f, f, t, t, t, f, f, f ] ] ]
      end ,
    f= fn[ i1 ] => [ ConnectedComponents.labelComponents i1 ] ,
    evaluate= 
      fn[ o1 ] =>
      let
        val truth = 
          IntGrayscaleImage.fromList[  
            [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
            [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
            [ 1, 1, 1, 0, 0, 2, 2, 2, 0, 0, 0 ], 
            [ 0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0 ], 
            [ 0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0 ] ]
      in
        [ IntGrayscaleImage.equal( o1, truth ) ]
      end ,
    inputToString = BooleanImage.toString }
