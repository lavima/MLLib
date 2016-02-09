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

    val foldl = GrayscaleImageReal.foldl
    val modify = GrayscaleImageReal.modify
    val convolve = 
      GrayscaleImageReal.convolve ( ImageCommon.copy, ImageCommon.original ) 
    val transposed = GrayscaleImageReal.transposed

    val sub = GrayscaleImageReal.sub
    val sub' = GrayscaleImageReal.sub'

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
                  ( image : GrayscaleImageReal.image ) 
        : BooleanImage.image = 
    let
      val { width, height, ... } = image

      val capX = cap width
      val capY = cap height


      val gaussian = FilterUtil.createGaussianMask( sigma )
      val gaussianDerived = ImageUtil.gradientXReal gaussian

      val ( sumPos, sumNeg ) = 
        foldl 
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
        modify
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

      val magnitude = GrayscaleImageReal.zeroImage( width, height )
      val _ =
        GrayscaleImageReal.modifyi
          ( fn( i, _ ) => 
              Math.sqrt( 
                Math.pow( sub'( gradX, i ), 2.0 ) +
                Math.pow( sub'( gradY, i ), 2.0 ) ) )
          magnitude

      val normalizedMagnitude = ImageUtil.normalizeReal magnitude

      val max = GrayscaleImageReal.zeroImage( width, height )
      val _ = 
        GrayscaleImageReal.modifyxy
          ( fn( x, y, _ ) => 
            let
              val dx = sub( gradX, x, y )
              val dy = sub( gradY, x, y )
              val m = sub( normalizedMagnitude, x, y )
            in
              if ( dy<=0.0 andalso dx>( ~dy ) ) orelse 
                 ( dy>=0.0 andalso dx<( ~dy ) ) then
              let
                val t = Real.abs( dy/dx )
                val m1 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, capX( x+1 ), y ),
                    sub( normalizedMagnitude, capX( x+1 ), capY( y-1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, capX( x-1 ), y ),
                    sub( normalizedMagnitude, capX( x-1 ), capY( y+1 ) ), 
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
                    sub( normalizedMagnitude, x, capY( y-1 ) ),
                    sub( normalizedMagnitude, capX( x+1 ), capY( y-1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, x, capY( y+1 ) ),
                    sub( normalizedMagnitude, capX( x-1 ), capY( y+1 ) ),
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
                    sub( normalizedMagnitude, x, capY( y-1 ) ),
                    sub( normalizedMagnitude, capX( x-1 ), capY( y-1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, x, capY( y+1 ) ),
                    sub( normalizedMagnitude, capX( x+1 ), capY( y+1 ) ),
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
                    sub( normalizedMagnitude, capX( x-1 ), y ),
                    sub( normalizedMagnitude, capX( x-1 ), capY( y-1 ) ),
                    t )
                val m2 = 
                  MathUtil.lerp( 
                    sub( normalizedMagnitude, capX( x+1 ), y ),
                    sub( normalizedMagnitude, capX( x+1 ), capY( y+1 ) ),
                    t )
              in
                if m>=m1 andalso m>=m2 then
                  m
                else
                  0.0
              end 
            end )
          max

      val ( high, low ) = 
        case options of 
          highLow( high, low ) => ( high, low )
        | highPercentageLowRatio( highPercentage, lowRatio ) => (
          let
            val [ high ] = 
              GrayscaleImageReal.thresholds'
                ( ImageCommon.percentage( 256, highPercentage ) )
                normalizedMagnitude
          in
            ( high, high*lowRatio )
          end )
        | otsuHighLowRatio lowRatio => 
          let
            val [ high ] =
              GrayscaleImageReal.thresholds'
                ( ImageCommon.otsu( 256 ) )
                normalizedMagnitude
          in
            ( high, high*lowRatio )
          end

      val edge = BooleanImage.zeroImage( width, height )
      val _ = GrayscaleImageReal.appxy
        ( fn( x, y, m ) =>
          let

            fun check( x : int, y : int ) : bool =
              if x=( capX x ) andalso y=( capY y ) then 
                let
                  val m' = sub( max, x, y )
                  val e = BooleanImage.sub( edge, x, y )
                in
                  ( not e andalso m'>low )
                end 
              else
                false

            fun follow( x : int, y : int ) : unit = 
              ( if check( x+1, y ) then
                  ( BooleanImage.update( edge, x+1, y, true );
                    follow( x+1, y ) )
                else
                  () ;
                if check( x+1, y+1 ) then
                  ( BooleanImage.update( edge, x+1, y+1, true );
                    follow( x+1, y+1 ) )
                else 
                  () ;
                if check( x, y+1 ) then
                  ( BooleanImage.update( edge, x, y+1, true );
                    follow( x, y+1 ) )
                else
                  () ;
                if check( x-1, y+1 ) then
                  ( BooleanImage.update( edge, x-1, y+1, true );
                    follow( x-1, y+1 ) )
                else
                  () ;
                if check( x-1, y ) then
                  ( BooleanImage.update( edge, x-1, y, true );
                    follow( x-1, y ) )
                else
                  () ;
                if check( x-1, y-1 ) then
                  ( BooleanImage.update( edge, x-1, y-1, true );
                    follow( x-1, y-1 ) )
                else
                  () ;
                if check( x, y-1 ) then
                  ( BooleanImage.update( edge, x, y-1, true );
                    follow( x, y-1 ) )
                else
                  () ;
                if check( x+1, y-1 ) then
                  ( BooleanImage.update( edge, x+1, y-1, true );
                    follow( x+1, y-1 ) )
                else
                  () )
            
            val e = BooleanImage.sub( edge, x, y )

          in
            if not e andalso m>high then
              ( BooleanImage.update( edge, x, y, true ); 
                follow( x, y ) )
            else
              ()
          end )
        max

    in
      edge 
    end

    val findEdges = 
      findEdges' ( Math.sqrt 2.0, highPercentageLowRatio( 0.7, 0.4 ) )
    

  end (* local *)

end (* structure Canny *)
