(*
* file: test_differential_evolution.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file validates the differential evolution implementation.
*)

val _ = print"\n\n********** Differential evolution tests **********\n"

val _ = UnitTest.test( "Optimize sphere function items",
  fn() => 
  let
    val randState = Random.rand( 1, 1 )

    fun fitness( alleles : real vector ) : real = 
      ~( Vector.foldl ( fn ( all, a ) => all*all + a ) 0.0 alleles )

    val min = Vector.fromList [ 0.0, 0.0, 0.0, 0.0, 0.0 ]
    val max = Vector.fromList [ 1.0, 1.0, 1.0, 1.0, 1.0 ]

    val optimized = DifferentialEvolution.optimize(
     fitness, 
     DifferentialEvolution.mutateDERandn( randState, 1, 0.4 ),
     5,
     50,
     200,
     0.5,
     min,
     max )
     randState
  in
    optimized
  end,
  fn x => 
  let
    val _ = print( Real.toString( #fitness x ) )
  in
    true
  end
  )
