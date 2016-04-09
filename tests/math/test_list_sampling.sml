(* 
* file: test_list_sampling.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains tests that validate the list sampling functionality
*)


val _ = print"\n\n********** List sampling tests **********\n"

val _ = UnitTest.test( "Sample k items",
  fn() => 
  let
    val items = [ 3, 4, 66, 8, 5, 2, 1, 4, 6, 4, 2 ]
    val randomState = Random.rand ( 1, 1 )
  in
    ListSampling.sampleK( randomState, items, 5 )
  end,
  fn x => List.length x = 5
  )
