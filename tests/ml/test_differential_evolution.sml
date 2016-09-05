(*
* file: test_differential_evolution.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file validates the differential evolution implementation.
*)

val _ = 
  SimpleTest.test' ( CommandLine.arguments() ) {
    group="DifferentialEvolution", what="Optimize sphere function items",
    genInput=
      fn() =>
        [ ( Real.~ o Vector.foldl ( fn ( all, a ) => all*all + a ) 0.0,
            DifferentialEvolution.mutateDERandn( Random.rand( 1, 1 ), 1, 0.4 ),
            5, 
            50,
            200,
            0.5,
            Vector.fromList [ 0.0, 0.0, 0.0, 0.0, 0.0 ],
            Vector.fromList [ 1.0, 1.0, 1.0, 1.0, 1.0 ] ) ] ,
    f= 
      fn[ i1 ] => 
        [ DifferentialEvolution.optimize i1 ( Random.rand( 1, 1 ) ) ] ,
    evaluate=
      fn[ o1 ] => 
      let
        val _ = print( Real.toString( #fitness o1 ) )
      in
        [ true ]
      end ,
    inputToString=
      fn( _, _, d, s, m, c, _, _ ) =>
        "( " ^
        Int.toString d ^ ", " ^
        Int.toString s ^ ", " ^
        Int.toString m ^ ", " ^
        Real.toString c ^ 
        " )" }
