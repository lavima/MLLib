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

  local

    fun createIndividual( fitness : real vector -> real, 
                          alleles : real vector ) : individual =
     { alleles = alleles, fitness = fitness alleles }
  in

   (*
    * Implementation of Differential evolution according.
    * 
    * Algorithm reference:
    *   Das, Swagatam, and Ponnuthurai Nagaratnam Suganthan. 
    *   "Differential evolution: a survey of the state-of-the-art." 
    *   Evolutionary Computation, IEEE Transactions on 15.1 (2011): 4-31.
    *)
		
   fun optimize( 
       fitness : real vector -> real, 
       mutate : ( ( real vector -> real ) * individual list ) -> individual,
       individualDim : int, 
       populationSize : int, 
       maxGenerations : int,
       crossoverFactor : real,
       min : real vector,
       max : real vector ) 
       ( rand : Random.rand ) 
     : individual = 
   let
     val population = List.tabulate( populationSize, 
         ( fn _ => 
           createIndividual(
             fitness, 
             RandomUtil.randomRealVectorRange( individualDim, min, max ) rand )
         ) ) 
     
     fun evolve( generation : int, population : individual list ) : individual =
     if generation>=maxGenerations then 
       List.foldl 
         ( fn ( toTest, best ) => 
          if ( #fitness toTest )>( #fitness best ) then toTest
          else best )
         ( List.hd population )
         ( population )
     else
       let
         val donors = List.tabulate( populationSize, 
           fn _ => mutate( fitness, population ) )
 
         val trial = ListPair.foldl
           ( fn ( p, d, a ) => 
             ( if ( Random.randReal rand ) <= crossoverFactor then d 
               else p  ) :: a ) 
           ( [] ) 
           ( population, donors )
 
         val nextPopulation = ListPair.foldl
           ( fn ( p, d, a ) => 
             ( if ( #fitness p ) >= ( #fitness d ) then p else d  ) :: a ) 
           ( [] ) 
           ( population, trial )

       in
         evolve( generation+1 , nextPopulation )
       end
 
      in
      evolve( 0, population )
   end

   (*
    * DE/rand/n mutation scheme.
    *)
   fun mutateDERandn ( randState : Random.rand, n : int, scaleFactor : real )
                     ( fitness : real vector -> real, 
                       population : individual list ) 
   : individual =
   let
     fun calculatePart( base, [] ) : real vector = base
       | calculatePart( base,  x::y::rest : individual list ) : real vector = 
         calculatePart( 
          VectorMath.addReal( base, 
            VectorMath.multiplyScalarReal(
             VectorMath.subtractReal( #alleles x, #alleles y ), scaleFactor ) ),
          rest )

     val first::rest = ListSampling.sampleK( randState, population, 1+2*n )
   in
     createIndividual( fitness, calculatePart( #alleles first, rest ) )
   end

  end
  
end

