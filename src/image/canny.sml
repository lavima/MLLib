(*
* filename: canny.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with an implementation of the Canny edge 
* detector.
*)

structure Canny =
struct
  
  datatype thresholdOptions = 
      highLow of real * real                (* Specify thresholds directly *)
    | highPercentageLowRatio of real * real (* Specify percentage of pixels for 
                                               calculating the high threshold 
                                               and the ratio of the high 
                                               threshold to use as the low
                                               threshold *)
    | otsuHighLowRatio of real              (* Use Otsu's method to determine
                                               the high threshold then 
                                               use the ratio to determine 
                                               the low threshold *)
  

  local

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
      
  in

    fun findEdges'( sigma : real, options : thresholdOptions )
                  ( image : RealGrayscaleImage.image ) 
        : BooleanImage.image = 
    let
      val ( height, width ) = RealGrayscaleImage.dimensions image

      val capX = cap width
      val capY = cap height

      val gaussian = FilterUtil.createGaussianMask sigma 
      val gaussianDerived = ImageUtil.gradientXReal gaussian

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

      val max = RealGrayscaleImage.zeroImage( height, width )
      val _ = 
        RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor
          ( fn( y, x, _ ) => 
            let
              val dx = sub( gradX, y, x )
              val dy = sub( gradY, y, x )
              val m = sub( normalizedMagnitude, y, x )
            in
              if ( dy<=0.0 andalso dx>( ~dy ) ) orelse 
                 ( dy>=0.0 andalso dx<( ~dy ) ) then
              let
                val t = Real.abs( dy/dx )
                val m1 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, y, capX( x+1 ) ),
                    sub( normalizedMagnitude, capY( y-1 ), capX( x+1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, y, capX( x-1 ) ),
                    sub( normalizedMagnitude, capY( y+1 ), capX( x-1 ) ), 
                    t )
              in
                if m>=m1 andalso m>=m2 then
                  m
                else
                  0.0
              end 
              else if ( dx>0.0 andalso ~dy>=dx ) orelse 
                      ( dx<0.0 andalso ~dy<=dx ) then 
              let
                val t = Real.abs( dx/dy )
                val m1 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, capY( y-1 ), x ),
                    sub( normalizedMagnitude, capY( y-1 ), capX( x+1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, capY( y+1 ), x ),
                    sub( normalizedMagnitude, capY( y+1 ), capX( x-1 ) ),
                    t )
              in
                if m>=m1 andalso m>=m2 then
                  m
                else
                  0.0
              end 
              else if ( dx<=0.0 andalso dx>dy ) orelse 
                      ( dx>=0.0 andalso dx<dy ) then 
              let
                val t = Real.abs( dx/dy )
                val m1 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, capY( y-1 ), x ),
                    sub( normalizedMagnitude, capY( y-1 ), capX( x-1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, capY( y+1 ), x ),
                    sub( normalizedMagnitude, capY( y+1 ), capX( x+1 ) ),
                    t )
              in
                if m>=m1 andalso m>=m2 then
                  m
                else
                  0.0
              end 
              else 
              let
                val t = Real.abs( dy/dx )
                val m1 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, y, capX( x-1 ) ),
                    sub( normalizedMagnitude, capY( y-1 ), capX( x-1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, y, capX( x+1 ) ),
                    sub( normalizedMagnitude, capY( y+1 ), capX( x+1 ) ),
                    t )
              in
                if m>=m1 andalso m>=m2 then
                  m
                else
                  0.0
              end 
            end )
          ( RealGrayscaleImage.region( 
              max, 
              1, 1, 
              SOME( height-2 ), SOME( width-2 ) ) )

      val ( high, low ) = 
        case options of 
          highLow( high, low ) => ( high, low )
        | highPercentageLowRatio( highPercentage, lowRatio ) => (
          let
            val high = 
              RealGrayscaleThreshold.percentage( 
                normalizedMagnitude, 
                256, 
                highPercentage )
                
          in
            ( high, high*lowRatio )
          end )
        | otsuHighLowRatio lowRatio => 
          let
            val high =
              RealGrayscaleThreshold.otsu( normalizedMagnitude, 256 )
          in
            ( high, high*lowRatio )
          end

      val edge = BooleanImage.zeroImage( height, width )
      val _ = 
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
      findEdges' ( Math.sqrt 2.0, highPercentageLowRatio( 0.7, 0.4 ) )
    

  end (* local *)

end (* structure Canny *)
