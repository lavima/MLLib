(*
* file: k_means.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains an implementation of the K-means algorithm.
*)

structure KMeans =
struct

  fun cluster( k : int, 
               numDimensions : int, 
               instances : real list list, 
               maxIterations : int ) 
      : int list * real list list =
  let
    val rand = Random.rand( 31, 29 )
    val means = Array.array( k, [] )
    val assignment = Array.array( List.length instances, ~1 )

    val _ = 
      Array.modify
        ( fn _ => 
            List.tabulate( numDimensions, fn _ => Random.randReal rand ) )
        means

    fun distance( instance : real list, meanIndex : int ) : real = 
      List.foldl
        ( fn( ( x, y ), accumDist ) => 
          let
            val dist = x-y 
          in
            accumDist+dist*dist
          end )
        0.0
        ( ListUtil.combine( instance, Array.sub( means, meanIndex ) ) )

    fun assign( i : int, instances' : real list list, changed : bool ) : bool =
      case instances' of
        [] => changed
      | instance::instances'' => 
        let
          val ( dist, meanIndex ) = 
            Util.accumLoop
              ( fn( j, ( minDist, minIndex ) ) =>
                let
                  val dist = distance( instance, j ) 
                in
                  if dist<minDist then
                    ( dist, j )
                  else
                    ( minDist, minIndex )
                end )
              ( Real.posInf, ~1 )
              k
          
          val prevMean = Array.sub( assignment, i )
        in
          if not( prevMean=meanIndex ) then (
            Array.update( assignment, i, meanIndex );
            assign( i+1, instances'', true ) )
          else
            assign( i+1, instances'', changed )
        end

    fun updateMeans() : unit = 
    let
      val sets = Array.array( k, [] )
      val _ =
        ListUtil.appi
          ( fn( i, instance ) =>
            let
              val meanIndex = Array.sub( assignment, i )
            in
              Array.update( sets, meanIndex, 
                instance::( Array.sub( sets, meanIndex ) ) )
            end ) 
          instances

      fun update( i : int ) : unit =
        case i<k of 
          false => ()
        | true => ( 
            Array.update( means, i,
              Util.avg 
                ( ListUtil.binaryOp ( Real.+ ), 
                  fn( xs, n ) => List.map ( fn x => x/real n ) xs,
                  fn _ => ( List.tabulate( numDimensions, fn _ => 0.0 ) ) )
                ( Array.sub( sets, i ) ) );
            update( i+1 ) )
    in
      update 0 
    end

    fun iterate( i : int ) : unit =
      case i<maxIterations of 
        false => ()
      | true => 
          case assign( 0, instances, false ) of
            false => ()
          | true => ( updateMeans(); iterate( i+1 ) )

    val _ = iterate 0

  in
    ( Array.foldr ( fn( x, xs ) => x::xs ) [] assignment, 
      Array.foldr ( fn( xs, xss ) => xs::xss ) [] means )
  end

end (* structure KMeans *)
