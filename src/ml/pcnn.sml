(*
* file: pcnn.sml
* author: Marius Geitle <marius.geitle@hiof.no>
* 
* This file contains an implementation of the pulse coupled neural network 
* algorithm.
*)

structure PCNN =
struct
  type pcnn = {
    height : int,
    width : int,

    f : real Array.array,
    l : real Array.array,
    y : bool Array2.array,
    t : real Array.array,

    feedbackDecay : real,
    linkDecay : real,
    thresholdDecay : real,
    beta : real,
    feedbackNormalization : real,
    linkNormalization : real,
    thresholdNormalization : real,

    linkWeightFun : real -> real
  }

  fun create( height : int, 
              width : int,     
              feedbackDecay : real,
              linkDecay : real,
              thresholdDecay : real,
              beta : real,
              feedbackNormalization : real,
              linkNormalization : real,
              thresholdNormalization : real,
              linkWeightFun : real -> real )
    : pcnn =
    {
       height = height, width = width,
       f = Array.array( height*width, 0.0 ),    
       l = Array.array( height*width, 0.0 ),
       y = Array2.array( height, width, false ),
       t = Array.array( height*width, 0.0 ),

       feedbackDecay = feedbackDecay,
       linkDecay = linkDecay,
       thresholdDecay = thresholdDecay,
       beta = beta,
       feedbackNormalization = feedbackNormalization,
       linkNormalization = linkNormalization,
       thresholdNormalization = thresholdNormalization,
       linkWeightFun = linkWeightFun
    }

  local
    
    fun calculateDistanceLists ( 
                                 height : int, 
                                 width : int,
                                 maxDistance : real )
      : ( int * real ) list list =
    let
      fun index_to_coords( i: int ) : int * int =
        ( i mod height, i div height )

      fun coord_to_index( y : int, x : int ) : int = x*height+y

      
      fun distanceList( x : int, y : int ) : ( int * real ) list =
        ( ( List.filter ( fn ( i , dist ) => dist<=maxDistance ) ) o
        ( List.map Option.valOf ) o
        ( List.filter Option.isSome ) o
        ( List.foldr op@ [] ) )
        ( let
            val floor_max = Real.floor maxDistance 
            fun d(x : int, y : int) : real = Math.sqrt( real( x*x+y*y ) )
           in
             List.tabulate
             ( Int.toInt( floor_max*2+1 ),
               fn j : int =>
                 List.tabulate ( floor_max*2+1,
                 fn i : int =>
                   if i-floor_max=0 andalso j-floor_max=0 then NONE 
                   else if i-floor_max+x>=0 andalso i-floor_max+x<width andalso
                           j-floor_max+y>=0 andalso j-floor_max+y<height then
                      SOME (
                        coord_to_index( i-floor_max+x, j-floor_max+y ),
                        d( i-floor_max, j-floor_max ) )
                      else
                      NONE ) )
          end )
  in
    List.tabulate( width*height, fn i => distanceList( index_to_coords i ) )
  end

  in

  fun fastLinkingIterate( pcnn : pcnn, 
                          neighbourhoodSize : real,
                          maxIterations : int,
                          stimulation : real Array2.array ) 
    : unit =
  let
    val  {
       height = height, width = width,
       f = f, l = l, y = y, t = t,
       feedbackDecay = feedbackDecay,
       linkDecay = linkDecay,
       thresholdDecay = thresholdDecay,
       beta = beta,
       feedbackNormalization = feedbackNormalization,
       linkNormalization = linkNormalization,
       thresholdNormalization = thresholdNormalization,
       linkWeightFun = linkWeightFun
    } = pcnn

    val totalSize = height*width
    val neighbourDistances = 
      calculateDistanceLists( height, width, neighbourhoodSize )
    val temp = Array.array( totalSize, 0.0 )

    fun index_to_coords( i: int ) : int * int =
      ( i mod height, i div height )

    fun coord_to_index( y : int, x : int ) : int = x*height+y 

    fun subByIndex( a, i : int ) = 
      Array2.sub( a, i mod height, i div height )

    fun updateByIndex( a, i : int, v ) : unit =
      Array2.update( a, i mod height, i div height, v )

    fun computeFastLinking( maxIterations : int ) : unit =
    let
      fun sum ( neighbours : ( bool * real ) list,
                weightFun : real -> real )
        : real =
        List.foldl 
          ( fn ( ( act, d ), a ) => if act then ( weightFun d )+a else a )
          ( 0.0 )
          ( neighbours )

      fun updateTmpYL( i : int, changed : bool ) : bool =
        if i=totalSize then changed
        else
          let
            val neighbourActivations : ( bool * real ) list = List.map 
              ( fn ( j, x ) => ( subByIndex( y, j ), x ) )
              ( List.nth( neighbourDistances, i ) )

            val oldY = subByIndex( y, i )
            val lVal = Array.sub( l, i )
            val tVal = Array.sub( t, i )
            val sVal = subByIndex( stimulation, i )

            val newL = linkDecay*lVal+linkNormalization*
                       ( sum( neighbourActivations, linkWeightFun ) )
            val u = sVal*( 1.0+beta*newL )
            val newY = tVal<u
            val newT = if newY then thresholdDecay*tVal+thresholdNormalization
                       else thresholdDecay*tVal

            val _ = updateByIndex( y, i, newY )
            val _ = Array.update( temp, i, newT )
            val _ = Array.update( l, i, newL )
          in
            updateTmpYL( i+1, changed orelse oldY<>newY )
          end

      fun updateT ( i : int ) = Array.copy { src = temp, dst = t, di = 0 }

      val changed = updateTmpYL( 0, false )
    in
      if changed andalso maxIterations>=1 then
        computeFastLinking( maxIterations-1 )
      else
        updateT 0
    end
  in
    computeFastLinking maxIterations
  end

  end

end
