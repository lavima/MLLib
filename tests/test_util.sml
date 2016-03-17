(* 
* file: test_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains tests that validate the functionality in the Util 
* structure.
*)


val _ = print"\n\n********** Util Tests **********\n"

val _ = test( "Testing Util.approxEqReal",
  fn() =>
    ( Util.approxEqReal( 1.001, 1.002, 2 ),
      Util.approxEqReal( 1.001, 1.002, 3 ) ),
  fn( X, Y ) => X andalso not Y )

val _ = test( "Testing Util.approxEqReal'",
  fn() =>
    ( Util.approxEqReal'( 1.124546548778789,  1.124546548778788, 15 ),
      Util.approxEqReal'( 1.124546548778789,  1.124546548778788, 16 ),
      Util.approxEqReal'( 1.124546548778789,  1.124546548778789, 16 )),
  fn( X, Y, Z ) => X andalso not Y andalso Z )

val _ = test( "Testing Util.loop",
  fn() =>
  let
    val Indices = Array.array( 10, 0 )
    val Outcome = 
      ( Util.loop ( fn I => Array.update( Indices, I, I ) ) 10;
        Array.foldli 
          ( fn( I, I', Correct ) => 
              if I=I' then
                Correct
              else
                false )
        true
        Indices )
      handle Subscript => false
  in
    Outcome
  end,
  fn X => X )

val _ = test( "Testing Util.max with reals",
  fn() => 
    ( Util.max Real.< [ ~1.0, 2.0, 3.0, ~10.0 ],
      Util.max Real.< [ 100.0, 10.0, 1.0 ],
      Util.max Real.< [ ~1.0, ~2.0, ~1.5 ] ),
  fn( X, Y, Z ) => 
    Real.==( X, 3.0 ) andalso
    Real.==( Y, 100.0 ) andalso
    Real.==( Z, ~1.0 ) )

val _ = test( "Testing Util.maxInt",
  fn() => 
    ( Util.maxInt[ ~1, 2, 3, ~10 ],
      Util.maxInt[ 100, 10, 1 ],
      Util.maxInt[ ~1, ~2, ~3 ] ),
  fn( X, Y, Z ) => X=3 andalso Y=100 andalso Z= ~1 )

val _ = test( "Testing Util.minInt",
  fn() => 
    ( Util.minInt[ ~1, 2, 3, ~10 ],
      Util.minInt[ 100, 10, 1 ],
      Util.minInt[ ~1, ~2, ~3 ] ),
  fn( X, Y, Z ) => X= ~10 andalso Y=1 andalso Z= ~3 )

val _ = test( "Testing Util.max2 with reals",
  fn() => 
    ( Util.max2 Real.< ( 3.0, ~4.0 ),
      Util.max2 Real.< ( ~3.0, 4.0 ),
      Util.max2 Real.< ( 3.0, 4.0 ),
      Util.max2 Real.< ( ~3.0, ~4.0 ) ),
  fn( X, Y, Z, W ) => 
    Real.==( X, 3.0 ) andalso
    Real.==( Y, 4.0 ) andalso
    Real.==( Z, 4.0 ) andalso
    Real.==( W, ~3.0 ) )

val _ = test( "Testing Util.max2Int",
  fn() => 
    ( Util.max2Int( 3, ~4 ),
      Util.max2Int( ~3, 4 ),
      Util.max2Int( 3, 4 ),
      Util.max2Int( ~3, ~4 ) ),
  fn( X, Y, Z, W ) => X=3 andalso Y=4 andalso Z=4 andalso W= ~3 )

val _ = test( "Testing Util.min2Int",
  fn() => 
    ( Util.min2Int( 3, ~4 ),
      Util.min2Int( ~3, 4 ),
      Util.min2Int( 3, 4 ),
      Util.min2Int( ~3, ~4 ) ),
  fn( X, Y, Z, W ) => X= ~4 andalso Y= ~3 andalso Z=3 andalso W= ~4 )
