(*
* file: differential_evolution.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file contains an implementation of differential evolution.
*)

structure DifferentialEvolution = 
struct
  type individual = {
    alleles : real vector,
    fitness : real
    } 
		
  fun optimize( 
      fitness : individual -> real, 
      mutate : ( ( individual -> real) * individual list ) -> individual,
      individualDim : int, 
      populationSize : int, 
      maxGenerations : int,
      crossoverFactor : real,
      min : real vector,
      max : real vector ) 
      ( rand : Random.rand ) 
    : individual = 
  let
    val population = List.tabulate(
        populationSize, 
        ( fn i => 
        let
          val a = RandomUtil.randomRealVectorRange(individualDim, min, max) 
                  rand
        in
          { alleles = a, fitness = fitness { alleles = a, fitness = 0.0 } }
        end ) )

    
    fun evolve( generation : int, population : individual list ) : individual =
    if generation>=maxGenerations then 
      List.foldl 
        ( fn ( toTest as { alleles = alleles, fitness = fitness }, 
               best as   { alleles = bestAlleles, fitness = bestFitness } ) => 
         if fitness > bestFitness then toTest
         else best )
        ( List.hd population )
        ( population )
    else
      let
        val donors = List.tabulate( populationSize, 
          fn i => mutate( fitness, population ) )

        val trial = ListPair.foldl
          ( fn ( p, d, a ) => 
            ( if ( Random.randReal rand ) <= crossoverFactor then d 
              else p  ) :: a ) 
          ( [] ) 
          ( population, donors )

        val nextPopulation = ListPair.foldl
          ( fn ( p, d, a ) => 
            ( if ( #fitness p ) <= ( #fitness d ) then p else d  ) :: a ) 
          ( [] ) 
          ( population, trial )

      in
        evolve( generation+1 , nextPopulation )
      end

     in
     evolve( 0, population )
  end

  (*
   * DE/rand/1 mutation scheme.
   *)
  fun mutateDERand1 ( randState : Random.rand, scaleFactor : real )
                    ( fitness : individual -> real, 
                       population : individual list ) 
  : individual =
  let
    val [ x1, x2, x3 ] = ListSampling.sampleK( randState, population, 3 )
    val a = 
      VectorMath.addReal( #alleles x1,
        VectorMath.multiplyScalarReal(
          VectorMath.subtractReal( #alleles x2, #alleles x1 ), scaleFactor ) )
  in
    { alleles = a, fitness = fitness { alleles = a, fitness = 0.0 } }    
  end
  
end

