(*
* file: k_means.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains an implementation of the K-means algorithm.
*)

structure KMeans =
struct

  fun cluster( K : int, 
               Dimensions : int, 
               Instances : real list list, 
               MaxIterations : int ) 
      : int list * real list list =
  let
    val Rand = Random.rand( 31, 29 )
    val Means = Array.array( K, [] )
    val Assignment = Array.array( List.length Instances, ~1 )

    val _ = 
      Array.modify
        ( fn _ => List.tabulate( Dimensions, fn _ => Random.randReal Rand ) )
        Means

    fun distance( Instance : real list, MeanIdx : int ) : real = 
      List.foldl
        ( fn( ( X, Y ), AccumDist ) => 
          let
            val Dist = X-Y 
          in
            AccumDist+Dist*Dist
          end )
        0.0
        ( ListUtil.combine( Instance, Array.sub( Means, MeanIdx ) ) )

    fun assign( I : int, Instances' : real list list, Changed : bool ) : bool =
      case Instances' of
        [] => Changed
      | Instance::RInstances' => 
        let
          val ( Distance, MeanIdx ) = 
            Util.accumLoop
              ( fn( J, ( MinDist, ClosestMean ) ) =>
                let
                  val Dist = distance( Instance, J ) 
                in
                  if Dist<MinDist then
                    ( Dist, J )
                  else
                    ( MinDist, ClosestMean )
                end )
              ( Real.posInf, ~1 )
              K
          
          val PrevMean = Array.sub( Assignment, I )
        in
          if not( PrevMean=MeanIdx ) then (
            Array.update( Assignment, I, MeanIdx );
            assign( I+1, RInstances', true ) )
          else
            assign( I+1, RInstances', Changed )
        end

    fun updateMeans() : unit = 
    let
      val Sets = Array.array( K, [] )
      val _ =
        ListUtil.appi
          ( fn( I, Instance ) =>
            let
              val MeanIdx = Array.sub( Assignment, I )
            in
              Array.update( Sets, MeanIdx, 
                Instance::( Array.sub( Sets, MeanIdx ) ) )
            end ) 
          Instances

      fun update( I : int ) : unit =
        case I<K of 
          false => ()
        | true => ( 
            Array.update( Means, I,
              Util.avg 
                ( ListUtil.binaryOp ( Real.+ ), 
                  fn( Xs, N ) => List.map ( fn X => X/real N ) Xs,
                  fn _ => ( List.tabulate( Dimensions, fn _ => 0.0 ) ) )
                ( Array.sub( Sets, I ) ) );
            update( I+1 ) )
    in
      update 0 
    end

    fun iterate( I : int ) : unit =
      case I<MaxIterations of 
        false => ()
      | true => 
          case assign( 0, Instances, false ) of
            false => ()
          | true => ( updateMeans(); iterate( I+1 ) )

    val _ = iterate 0

  in
    ( Array.foldr ( fn( X, Xs ) => X::Xs ) [] Assignment, 
      Array.foldr ( fn( Xs, Xss ) => Xs::Xss ) [] Means )
  end

end (* structure KMeans *)
