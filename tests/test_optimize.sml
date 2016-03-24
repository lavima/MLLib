(* 
* file: test_optimize.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the Optimize structure
*)

val _ = print"\n\n********** Optimize Tests **********\n"

val _ = UnitTest.test( "Testing Optimize.brute",
  fn() => 
    Optimize.brute 
      ( 3, [ Optimize.full 5, Optimize.full 10, Optimize.lessEqual 10 ] ) 
      ( fn Xs =>
        let
          val X1::X2::X3::nil = Xs
          val _ = 
            print( "X1: " ^ Real.toString X1 ^ " X2: " ^ Real.toString X2 ^ " X3: " ^ Real.toString X3 ^ "\n" )
        in
          X1*X2*X3
        end ) , 
  fn Xs => 
  let
    val X1::X2::X3::nil = Xs
    val _ = 
      print( "X1: " ^ Real.toString X1 ^ " X2: " ^ Real.toString X2 ^ " X3: " ^ Real.toString X3 ^ "\n" )
  in
    true
  end
  )
          
