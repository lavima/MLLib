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

    f : real Array2.array,
    l : real Array2.array,
    y : bool Array2.array,
    t : real Array2.array,

    feedingDecay : real,
    linkDecay : real,
    thresholdDecay : real,
    beta : real,
    feedingNormalization : real,
    linkNormalization : real,
    thresholdNormalization : real,

    feedingWeightFun : real -> real,
    linkWeightFun : real -> real
  }

  fun create( height : int, 
              width : int,     
              feedingDecay : real,
              linkDecay : real,
              thresholdDecay : real,
              beta : real,
              feedingNormalization : real,
              linkNormalization : real,
              thresholdNormalization : real,
              feedingWeightFun : real -> real,
              linkWeightFun : real -> real )
    : pcnn =
    {
       height = height, width = width,
       f = Array2.array( height, width, 0.0 ),    
       l = Array2.array( height, width, 0.0 ),
       y = Array2.array( height, width, false ),
       t = Array2.array( height, width, 0.0 ),

       feedingDecay = feedingDecay,
       linkDecay = linkDecay,
       thresholdDecay = thresholdDecay,
       beta = beta,
       feedingNormalization = feedingNormalization,
       linkNormalization = linkNormalization,
       thresholdNormalization = thresholdNormalization,
       feedingWeightFun = feedingWeightFun,
       linkWeightFun = linkWeightFun
    }

  local
    
    fun calculateDistanceLists ( height : int, 
                                 width : int,
                                 maxDistance : real )
      : ( int * int * real ) list Array2.array =
    let
      val floor_max = Real.floor maxDistance 
      fun d( y : int, x : int ) : real = Math.sqrt( real( x*x+y*y ) )
      
      fun distanceList( y : int, x : int ) : ( int * int * real ) list =
        ( ( List.filter ( fn ( i, j , dist ) => dist<=maxDistance ) ) o
        ( List.map Option.valOf ) o
        ( List.filter Option.isSome ) o
        ( List.foldr op@ [] ) )
        ( List.tabulate( floor_max*2+1, fn i =>
          List.tabulate( floor_max*2+1, fn j =>
          let
            val dy = y+i-( Real.floor maxDistance )
            val dx = x+j-( Real.floor maxDistance )
          in
            if dy<=0 orelse dx<=0 orelse dy>height-1 orelse dx>width-1 then NONE
            else SOME ( dy, dx, d( dy-y, dx-x ) )
          end ) ) )
    in
      Array2.tabulate Array2.RowMajor ( height, width, distanceList )
    end
  in

    fun fastLinkingIterate( pcnn : pcnn, 
                            neighbourhoodSize : real,
                            maxIterations : int,
                            stimulation : real Array2.array ) 
      : unit =
    let
      val {
         height = height, width = width,
         f = f, l = l, y = y, t = t,
         feedingDecay = feedingDecay,
         linkDecay = linkDecay,
         thresholdDecay = thresholdDecay,
         beta = beta,
         feedingNormalization = feedingNormalization,
         linkNormalization = linkNormalization,
         thresholdNormalization = thresholdNormalization,
         feedingWeightFun = feedingWeightFun,
         linkWeightFun = linkWeightFun
      } = pcnn

      val neighbourDistances = 
        calculateDistanceLists( height, width, neighbourhoodSize )
      val temp = Array2.array( height, width, 0.0 )

      fun sum ( stim : real, neighbours : ( bool * real ) list, weightFun : real -> real )
        : real =
        List.foldl 
          ( fn ( ( act, d ), a ) => if act then ( weightFun d )+a 
                                    else stim+a )
          ( 0.0 )
          ( neighbours )

      fun updateT() = 
        Array2.copy {
          src = { base = temp, row = 0, col = 0, nrows = NONE, ncols = NONE }, 
          dst = t, dst_row = 0, dst_col = 0 }

      fun computeFastLinking( maxIterations : int ) : unit =
      let       
        fun updateTempYL( i : int, j : int, oldY : bool, changed : bool ) 
          : bool =
        let
          val neighbourActivations : ( bool * real ) list = List.map 
            ( fn ( i, j, x ) => ( Array2.sub( y, i, j ), x ) )
            ( Array2.sub( neighbourDistances, i, j ) )

          val fVal = Array2.sub( f, i, j )
          val lVal = Array2.sub( l, i, j )
          val tVal = Array2.sub( t, i, j )

          val newL = linkDecay*lVal+linkNormalization*
                       ( sum(fVal, neighbourActivations, linkWeightFun ) )

          val u = fVal*( 1.0+beta*newL )
          val newY = tVal<u
          val newT = if newY then thresholdDecay*tVal+thresholdNormalization
                     else thresholdDecay*tVal

          val _ = Array2.update( y, i, j, newY )
          val _ = Array2.update( temp, i, j, newT )
          val _ = Array2.update( l, i, j, newL )
        in
          changed orelse oldY<>newY
        end

        val changed = Array2.foldi Array2.RowMajor updateTempYL false 
          { base = y, row = 0, col = 0, nrows = NONE, ncols = NONE }
      in
        if changed andalso maxIterations>=1 then
          computeFastLinking( maxIterations-1 )
        else
          updateT()
      end
      
      fun updateFeeding( i : int, j : int, fVal : real ) : real =
      let
        val neighbourActivations : ( bool * real ) list = List.map 
          ( fn ( i, j, x ) => ( Array2.sub( y, i, j ), x ) )
          ( Array2.sub( neighbourDistances, i, j ) )

        val sVal = Array2.sub( stimulation, i, j )   
        val newF = feedingDecay*fVal+feedingNormalization*
                   ( sum(sVal, neighbourActivations, feedingWeightFun ) ) + sVal
      in
        sVal
      end

      val _ = Array2.modifyi Array2.RowMajor updateFeeding 
        { base = f, row = 0, col = 0, nrows = NONE, ncols = NONE }
    in
      computeFastLinking maxIterations
    end
  end
end
