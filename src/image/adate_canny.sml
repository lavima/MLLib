(*
* filename: adate_canny.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with implementation of the improved Canny edge 
* detector. See http://www.it.hiof.no/iaml/ for details.
*)


structure ADATECanny =
struct

  datatype improvement =
    filterMask | 
    nonMaxSuppression | 
    hysteresisThresholding 

  
  local

    val member = ListUtil.member ( fn( a, b ) => a=b )

    val fold = RealGrayscaleImage.fold
    val modify = RealGrayscaleImage.modify
    val convolve = 
      RealGrayscaleImage.convolve 
        ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize ) 
    val transposed = RealGrayscaleImage.transposed

    val sub = RealGrayscaleImage.sub

    fun cap ( max : int ) 
            ( x : int ) 
        : int =
      if x>=max then
        max-1
      else if x<0 then
        0
      else
        x

    fun check ( max : int ) 
              ( x : int )
        : bool =
      if x>=max then
        false
      else if x<0 then
        false
      else
        true

      
    fun createGaussianMask( sigma : real ) : RealGrayscaleImage.image = 
    let
      val l = Real.realRound( 10.01*Real.realCeil sigma )
      val n = ( l-1.0 )/2.0
      val xs = ListUtil.fromToReal( ~n, n )

      val mask = RealGrayscaleImage.zeroImage( 1, List.length xs )

      val c = 1.0/( Math.sqrt( 2.0*Math.pi )*sigma )

      fun gaussian( x : real ) : real = 
        c *
        Math.exp( 
          ~( ( x*x )/( ( ( x/Math.tanh x )*sigma )+( ( sigma*sigma )-sigma ) ) ) ) 

      val _ = 
        List.foldl
          ( fn( x, i ) =>
            let
              val _ = RealGrayscaleImage.update( mask, 0, i, gaussian x ) 
            in
              i+1
            end )
          0
          xs

      val sum = 
        RealGrayscaleImage.fold RealGrayscaleImage.RowMajor 
          ( fn( x, s ) => s+x ) 
          0.0 
          mask
      val _ = 
        RealGrayscaleImage.modify RealGrayscaleImage.RowMajor 
          ( fn x => x/sum ) 
          mask

    in
      mask
    end

    fun gradientX( im : RealGrayscaleImage.image )
        : RealGrayscaleImage.image =
    let

      val sub = RealGrayscaleImage.sub
      val ( height, width ) = RealGrayscaleImage.dimensions im

      val out = RealGrayscaleImage.zeroImage( height, width )
      val _ = 
        RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor
          ( fn( y, x, _ ) => 
              if x=0 then 
                Math.tanh( 0.0-sub( im, y, x ) )
              else if x=width-1 then
                Math.tanh( 0.0-sub( im, y, x-1 ) )
              else
                ( sub( im, y, x+1 )-Math.tanh( sub( im, y, x ) ) )/2.0 )
          ( RealGrayscaleImage.full out )
    in
      out
    end

  in

    fun findEdges'( improvements : improvement list )
                  ( sigma : real, options : Canny.thresholdOptions )
                  ( image : RealGrayscaleImage.image ) 
        : BooleanImage.image = 
    let
      val ( height, width ) = RealGrayscaleImage.dimensions image

      val capX = cap width
      val capY = cap height

      val improve = member( filterMask, improvements )
      val gaussian = 
        case improve of 
          false => FilterUtil.createGaussianMask sigma
        | true => createGaussianMask sigma 
      val gaussianDerived = 
        case improve of 
          false => ImageUtil.gradientXReal gaussian
        | true => gradientX gaussian

      val ( sumPos, sumNeg ) = 
        fold RealGrayscaleImage.RowMajor
          ( fn( x, ( sumPos, sumNeg ) ) => 
              if x > 0.0 then 
                ( sumPos+x, sumNeg ) 
              else if x < 0.0 then
                ( sumPos, sumNeg+x )
              else
                ( sumPos, sumNeg ) ) 
          ( 0.0, 0.0 )
          gaussianDerived
      val _ =
        modify RealGrayscaleImage.RowMajor
          ( fn x =>
              if x > 0.0 then 
                x/sumPos 
              else if x < 0.0 then
                x/( Real.abs sumNeg )
              else
                x )
          gaussianDerived

      val smoothX = convolve( image, transposed gaussian )
      val gradX = convolve( smoothX, gaussianDerived )

      val smoothY = convolve( image, gaussian )
      val gradY = convolve( smoothY, transposed gaussianDerived )

      val magnitude = RealGrayscaleImage.zeroImage( height, width )
      val _ =
        RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor
          ( fn( i, j, _ ) => 
              Math.sqrt( 
                Math.pow( sub( gradX, i, j ), 2.0 ) +
                Math.pow( sub( gradY, i, j ), 2.0 ) ) )
          ( RealGrayscaleImage.full magnitude )

      val normalizedMagnitude = ImageUtil.normalizeReal magnitude

      val improve = member( nonMaxSuppression, improvements )
      val max = RealGrayscaleImage.zeroImage( height, width )
      val _ = 
        RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor
          ( fn( y, x, _ ) => 
            let
              fun sub'( im, y, x ) = 0.5*sub( im, y, x )
              fun cvt x = 2.0*x

              val dx = sub'( gradX, y, x )
              val dy = sub'( gradY, y, x )
              val m = sub'( normalizedMagnitude, y, x )
            in
              if ( dy<=0.0 andalso dx>( ~dy ) ) orelse 
                 ( dy>=0.0 andalso dx<( ~dy ) ) then
              let
                val t = 
                  case improve of 
                    false => Real.abs( dy/dx )
                  | true => sub'( normalizedMagnitude, y, capX( x-1 ) )
                val m1 = 
                  MathUtil.lerp( 
                    sub'( normalizedMagnitude, y, capX( x+1 ) ),
                    sub'( normalizedMagnitude, capY( y-1 ), capX( x+1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub'( normalizedMagnitude, y, capX( x-1 ) ),
                    sub'( normalizedMagnitude, capY( y+1 ), capX( x-1 ) ), 
                    t )
              in
                if m>=m1 andalso m>=m2 then
                  case improve of
                    false => cvt m
                  | true => cvt( Real.abs( m/Math.tanh( m/dx ) ) )
                else
                  0.0
              end 
              else if ( dx>0.0 andalso ~dy>=dx ) orelse 
                      ( dx<0.0 andalso ~dy<=dx ) then 
              let
                val t = 
                  case improve of 
                    false => Real.abs( dx/dy )
                  | true => sub'( normalizedMagnitude, capY( y+1 ), x )
                val m1 = 
                  MathUtil.lerp( 
                    sub'( normalizedMagnitude, capY( y-1 ), x ),
                    sub'( normalizedMagnitude, capY( y-1 ), capX( x+1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub'( normalizedMagnitude, capY( y+1 ), x ),
                    sub'( normalizedMagnitude, capY( y+1 ), capX( x-1 ) ),
                    t )
              in
                if m>=m1 andalso m>=m2 then
                  case improve of
                    false => cvt m
                  | true => cvt( Real.abs( m/Math.tanh( m/dy ) ) )
                else
                  0.0
              end 
              else if ( dx<=0.0 andalso dx>dy ) orelse 
                      ( dx>=0.0 andalso dx<dy ) then 
              let
                val t =
                  case improve of 
                    false => Real.abs( dx/dy )
                  | true => sub'( normalizedMagnitude, capY( y+1 ), x )
                val m1 = 
                  MathUtil.lerp( 
                    sub'( normalizedMagnitude, capY( y-1 ), x ),
                    sub'( normalizedMagnitude, capY( y-1 ), capX( x-1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub'( normalizedMagnitude, capY( y+1 ), x ),
                    sub'( normalizedMagnitude, capY( y+1 ), capX( x+1 ) ),
                    t )
              in
                if m>=m1 andalso m>=m2 then
                  case improve of
                    false => cvt m
                  | true => cvt( Real.abs( m/Math.tanh( m/dy ) ) )
                else
                  0.0
              end 
              else 
              let
                val t = 
                  case improve of 
                    false => Real.abs( dy/dx )
                  | true => sub'( normalizedMagnitude, y, capX( x+1 ) )
                val m1 = 
                  MathUtil.lerp( 
                    sub'( normalizedMagnitude, y, capX( x-1 ) ),
                    sub'( normalizedMagnitude, capY( y-1 ), capX( x-1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub'( normalizedMagnitude, y, capX( x+1 ) ),
                    sub'( normalizedMagnitude, capY( y+1 ), capX( x+1 ) ),
                    t )
              in
                if m>=m1 andalso m>=m2 then
                  case improve of
                    false => cvt m
                  | true => cvt( Real.abs( m/Math.tanh( m/dx ) ) )
                else
                  0.0
              end 
            end )
          ( RealGrayscaleImage.full max )

      val ( high, low ) = 
        case options of 
          Canny.highLow( high, low ) => ( high, low )
        | Canny.highPercentageLowRatio( highPercentage, lowRatio ) => (
          let
            val high = 
              RealGrayscaleThreshold.percentage( 
                normalizedMagnitude, 
                256, 
                highPercentage )
                
          in
            ( high, high*lowRatio )
          end )
        | Canny.otsuHighLowRatio lowRatio => 
          let
            val high =
              RealGrayscaleThreshold.otsu( normalizedMagnitude, 256 )
          in
            ( high, high*lowRatio )
          end

      val edge = BooleanImage.zeroImage( height, width )
      val edgeTemp = BooleanImage.zeroImage( height, width )

      val improve = member( hysteresisThresholding, improvements )
      val _ = 
        if improve then
          RealGrayscaleImage.appi RealGrayscaleImage.RowMajor
            ( fn( y, x, m ) =>
              let
                
                datatype pos = pos of int * int
                datatype navResult = invalid | valid of pos
                datatype navType = 
                  up | upRight | right | downRight | 
                  down | downLeft | left | upLeft

                fun esub( e, pos( x, y ) ) = BooleanImage.sub( e, y, x )
                fun eup( e, pos( x, y ), v ) = BooleanImage.update( e, y, x, v )
                fun gsub( pos( x, y ) ) = RealGrayscaleImage.sub( max, y, x )

                val checkX = check width
                val checkY = check height

                fun nav( p, n ) : navResult = 
                let
                  fun wrap( p as pos( x', y' ) ) =
                    if checkX x' andalso checkY y' then
                      valid p
                    else
                      invalid
                in
                  case p of pos( x, y ) =>
                  case n of
                    up => wrap( pos( x, y-1 ) )
                  | upRight => wrap( pos( x+1, y-1 ) )
                  | right => wrap( pos( x+1, y ) )
                  | downRight => wrap( pos( x+1, y+1 ) )
                  | down => wrap( pos( x, y+1 ) )
                  | downLeft => wrap( pos( x-1, y+1 ) )
                  | left => wrap( pos( x-1, y ) )
                  | upLeft => wrap( pos( x-1, y-1 ) )
                end

                fun checkUpdate( e, et, p, t, us ) =
                  case esub( e, p ) of 
                    false => ( 
                      case esub( et, p ) of 
                        false => (
                          case t<gsub p of 
                            false => ( false, us )
                          | true => ( 
                              case eup( et, p, true ) of _ =>
                              ( true, p::us ) ) )
                        | true => ( false, us ) ) 
                  | true => ( false, us )

                fun updateAndClear( us ) =
                  case us of
                    [] => ()
                  | pos( x, y )::us' => (                  
                      BooleanImage.update( edge, y, x, true );
                      BooleanImage.update( edgeTemp, y, x, false );
                      updateAndClear(  us' ) )

                fun f( h, l, p, m ) =
                let
                  fun follow( fp, us ) =
                    let
                      fun follow'( ps, us' ) =
                        case ps of
                          [] => us'
                        | p::ps' =>
                        case checkUpdate( edge, edgeTemp, p, l, us ) 
                          of ( u', us'' ) =>
                        case u' of
                          false => (
                            case nav( p, downLeft ) of
                              invalid => us
                            | valid _ => follow'( ps', us' )
                            )
                        | true => follow( p, follow'( ps, us'' ) )
                    in
                      follow'(
                        let
                          fun filter ns =
                            case ns of
                              [] => []
                            | n::ns' =>
                            case nav( fp, n ) of
                              invalid => []
                            | valid p => p::filter ns'
                        in
                          filter[
                            downRight, upRight, right, 
                            downLeft, down, upLeft, 
                            left, upLeft, up ]
                        end ,
                        follow'( [ p ], us ) )
                    end 
                in
                  case h<m of
                    false => []
                  | true =>
                  case
                    checkUpdate(
                      edge,
                      edge,
                      p,
                      Math.tanh( h )*h,
                      f( l, m, p, h ) )  
                        of ( _, us ) => 
                      follow( p, us )
                end
              in
                updateAndClear( f( high, low, pos( x, y ), m ) )
              end )
          ( RealGrayscaleImage.full max )
        else
          RealGrayscaleImage.appi RealGrayscaleImage.RowMajor
            ( fn( y, x, m ) =>
              let

                fun check( x : int, y : int ) : bool =
                  if x=( capX x ) andalso y=( capY y ) then 
                    let
                      val m' = sub( max, y, x )
                      val e = BooleanImage.sub( edge, y, x )
                    in
                      ( not e andalso m'>low )
                    end 
                  else
                    false

                fun follow( x : int, y : int ) : unit = 
                  ( if check( x+1, y ) then
                      ( BooleanImage.update( edge, y, x+1, true );
                        follow( x+1, y ) )
                    else
                      () ;
                    if check( x+1, y+1 ) then
                      ( BooleanImage.update( edge, y+1, x+1, true );
                        follow( x+1, y+1 ) )
                    else 
                      () ;
                    if check( x, y+1 ) then
                      ( BooleanImage.update( edge, y+1, x, true );
                        follow( x, y+1 ) )
                    else
                      () ;
                    if check( x-1, y+1 ) then
                      ( BooleanImage.update( edge, y+1, x-1, true );
                        follow( x-1, y+1 ) )
                    else
                      () ;
                    if check( x-1, y ) then
                      ( BooleanImage.update( edge, y, x-1, true );
                        follow( x-1, y ) )
                    else
                      () ;
                    if check( x-1, y-1 ) then
                      ( BooleanImage.update( edge, y-1, x-1, true );
                        follow( x-1, y-1 ) )
                    else
                      () ;
                    if check( x, y-1 ) then
                      ( BooleanImage.update( edge, y-1, x, true );
                        follow( x, y-1 ) )
                    else
                      () ;
                    if check( x+1, y-1 ) then
                      ( BooleanImage.update( edge, y-1, x+1, true );
                        follow( x+1, y-1 ) )
                    else
                      () )
                
                val e = BooleanImage.sub( edge, y, x )

              in
                if not e andalso m>high then
                  ( BooleanImage.update( edge, y, x, true ); 
                    follow( x, y ) )
                else
                  ()
              end )
          ( RealGrayscaleImage.full max )

    in
      edge 
    end

    val findEdges = 
      findEdges' 
        [ filterMask, nonMaxSuppression, hysteresisThresholding ]
        ( Math.sqrt 2.0, Canny.highPercentageLowRatio( 0.7, 0.4 ) )
    

  end (* local *)

end (* structure Canny *)
