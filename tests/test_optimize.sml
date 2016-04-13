(* 
* file: test_optimize.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the Optimize structure
*)

val _ = print"\n\n********** Optimize Tests **********\n"

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="Optimize", what="Testing Optimize.brute",
    genInput = 
      fn() => [ 
        ( 3, [ Optimize.full 5, Optimize.full 10, Optimize.lessEqual 10 ] ) ] ,
    f = 
      fn[ i1 ] => [
        Optimize.brute i1
          ( fn xs =>
            let
              val x1::x2::x3::nil = xs
            in
              x1*x2*x3
            end ) ] , 
    evaluate = 
      fn[ o1 ] => 
      let
        val x1::x2::x3::nil = o1
      in
        [ true ]
      end ,
    inputToString = fn( x, xs ) => Int.toString x }

          
