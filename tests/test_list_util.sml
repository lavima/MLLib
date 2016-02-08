(* 
* file: test_list_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the functionality in the ListUtil 
* structure.
*)

val _ = print"\n\n************** ListUtil Tests **************\n"

val _ = test( "Testing ListUtil.equal",
  fn() => 
    ( ListUtil.equal Util.eqInt ( [], [] ),
      ListUtil.equal Util.eqInt ( [ 1, 2 ], [ 1, 2 ] ),
      ListUtil.equal Util.eqInt ( [ 3, 2 ], [ 2, 3 ] ),
      ListUtil.equal Util.eqInt ( [ 3, 2 ], [ 3 ] ) ),
  fn( X, Y, Z, W ) => X andalso Y andalso not Z andalso not W )

val _ = test( "Testing ListUtil.fromToInt",
  fn() => ListUtil.fromToInt( ~5, 5 ),
  fn Xs =>
    ListUtil.equal Util.eqInt 
      ( Xs, [ ~5, ~4, ~3, ~2, ~1, 0, 1, 2, 3, 4, 5 ] ) )

val _ = test( "Testing ListUtil.fromToReal",
  fn() => ListUtil.fromToReal( ~1.5, 2.5 ),
  fn Xs =>
    ListUtil.equal Util.eqReal ( Xs, [ ~1.5, ~0.5, 0.5, 1.5, 2.5 ] ) )
