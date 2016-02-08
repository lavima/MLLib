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

    fun cap ( Max : int ) 
            ( X : int ) 
        : int =
      if X>=Max then
        Max-1
      else if X<0 then
        0
      else
        X
      
  in

    fun findEdges'( Sigma : real, Options : thresholdOptions )
                  ( Image : GrayscaleImageReal.image ) 
        : BooleanImage.image = 
    let
      val { Width, Height, Values } = Image

      val capX = cap Width
      val capY = cap Height


      val Gaussian = FilterUtil.createGaussianMask( Sigma )
      val GaussianDerived = ImageUtil.gradientXReal Gaussian

      val ( SumPos, SumNeg ) = 
        foldl 
          ( fn( X, ( SumPos, SumNeg ) ) => 
              if X > 0.0 then 
                ( SumPos+X, SumNeg ) 
              else if X < 0.0 then
                ( SumPos, SumNeg+X )
              else
                ( SumPos, SumNeg ) ) 
          ( 0.0, 0.0 )
          GaussianDerived
      val _ =
        modify
          ( fn X =>
              if X > 0.0 then 
                X/SumPos 
              else if X < 0.0 then
                X/( Real.abs SumNeg )
              else
                X )
          GaussianDerived

      val SmoothX = convolve( Image, transposed Gaussian )
      val GradX = convolve( SmoothX, GaussianDerived )

      val SmoothY = convolve( Image, Gaussian )
      val GradY = convolve( SmoothY, transposed GaussianDerived )

      val Magnitude = GrayscaleImageReal.zeroImage( Width, Height )
      val _ =
        GrayscaleImageReal.modifyi
          ( fn( I, _ ) => 
              Math.sqrt( 
                Math.pow( sub'( GradX, I ), 2.0 ) +
                Math.pow( sub'( GradY, I ), 2.0 ) ) )
          Magnitude

      val NormalizedMagnitude = ImageUtil.normalizeReal Magnitude

      val Max = GrayscaleImageReal.zeroImage( Width, Height )
      val _ = 
        GrayscaleImageReal.modifyxy
          ( fn( X, Y, _ ) => 
            let
              val DX = sub( GradX, X, Y )
              val DY = sub( GradY, X, Y )
              val M = sub( NormalizedMagnitude, X, Y )
            in
              if ( DY<=0.0 andalso DX>( ~DY ) ) orelse 
                 ( DY>=0.0 andalso DX<( ~DY ) ) then
              let
                val T = Real.abs( DY/DX )
                val M1 = 
                  MathUtil.lerp( 
                    sub( NormalizedMagnitude, capX( X+1 ), Y ),
                    sub( NormalizedMagnitude, capX( X+1 ), capY( Y-1 ) ),
                    T )
                val M2 = 
                  MathUtil.lerp( 
                    sub( NormalizedMagnitude, capX( X-1 ), Y ),
                    sub( NormalizedMagnitude, capX( X-1 ), capY( Y+1 ) ), 
                    T )
              in
                if M>=M1 andalso M>=M2 then
                  M
                else
                  0.0
              end 
              else if ( DX>0.0 andalso ~DY>=DX ) orelse 
                      ( DX<0.0 andalso ~DY<=DX ) then 
              let
                val T = Real.abs( DX/DY )
                val M1 = 
                  MathUtil.lerp( 
                    sub( NormalizedMagnitude, X, capY( Y-1 ) ),
                    sub( NormalizedMagnitude, capX( X+1 ), capY( Y-1 ) ),
                    T )
                val M2 = 
                  MathUtil.lerp( 
                    sub( NormalizedMagnitude, X, capY( Y+1 ) ),
                    sub( NormalizedMagnitude, capX( X-1 ), capY( Y+1 ) ),
                    T )
              in
                if M>=M1 andalso M>=M2 then
                  M
                else
                  0.0
              end 
              else if ( DX<=0.0 andalso DX>DY ) orelse 
                      ( DX>=0.0 andalso DX<DY ) then 
              let
                val T = Real.abs( DX/DY )
                val M1 = 
                  MathUtil.lerp( 
                    sub( NormalizedMagnitude, X, capY( Y-1 ) ),
                    sub( NormalizedMagnitude, capX( X-1 ), capY( Y-1 ) ),
                    T )
                val M2 = 
                  MathUtil.lerp( 
                    sub( NormalizedMagnitude, X, capY( Y+1 ) ),
                    sub( NormalizedMagnitude, capX( X+1 ), capY( Y+1 ) ),
                    T )
              in
                if M>=M1 andalso M>=M2 then
                  M
                else
                  0.0
              end 
              else 
              let
                val T = Real.abs( DY/DX )
                val M1 = 
                  MathUtil.lerp( 
                    sub( NormalizedMagnitude, capX( X-1 ), Y ),
                    sub( NormalizedMagnitude, capX( X-1 ), capY( Y-1 ) ),
                    T )
                val M2 = 
                  MathUtil.lerp( 
                    sub( NormalizedMagnitude, capX( X+1 ), Y ),
                    sub( NormalizedMagnitude, capX( X+1 ), capY( Y+1 ) ),
                    T )
              in
                if M>=M1 andalso M>=M2 then
                  M
                else
                  0.0
              end 
            end )
          Max

      val ( High, Low ) = 
        case Options of 
          highLow( High, Low ) => ( High, Low )
        | highPercentageLowRatio( HighPercentage, LowRatio ) => (
          let
            val [ High ] = 
              GrayscaleImageReal.thresholds'
                ( ImageCommon.percentage( 256, HighPercentage ) )
                NormalizedMagnitude
          in
            ( High, High*LowRatio )
          end )
        | otsuHighLowRatio LowRatio => 
          let
            val [ High ] =
              GrayscaleImageReal.thresholds'
                ( ImageCommon.otsu( 256 ) )
                NormalizedMagnitude
          in
            ( High, High*LowRatio )
          end

      val Edge = BooleanImage.zeroImage( Width, Height )
      val _ = GrayscaleImageReal.appxy
        ( fn( X, Y, M ) =>
          let

            fun check( X : int, Y : int ) : bool =
              if X=( capX X ) andalso Y=( capY Y ) then 
                let
                  val M' = sub( Max, X, Y )
                  val E = BooleanImage.sub( Edge, X, Y )
                in
                  ( not E andalso M'>Low )
                end 
              else
                false

            fun follow( X : int, Y : int ) : unit = 
              ( if check( X+1, Y ) then
                  ( BooleanImage.update( Edge, X+1, Y, true );
                    follow( X+1, Y ) )
                else
                  () ;
                if check( X+1, Y+1 ) then
                  ( BooleanImage.update( Edge, X+1, Y+1, true );
                    follow( X+1, Y+1 ) )
                else 
                  () ;
                if check( X, Y+1 ) then
                  ( BooleanImage.update( Edge, X, Y+1, true );
                    follow( X, Y+1 ) )
                else
                  () ;
                if check( X-1, Y+1 ) then
                  ( BooleanImage.update( Edge, X-1, Y+1, true );
                    follow( X-1, Y+1 ) )
                else
                  () ;
                if check( X-1, Y ) then
                  ( BooleanImage.update( Edge, X-1, Y, true );
                    follow( X-1, Y ) )
                else
                  () ;
                if check( X-1, Y-1 ) then
                  ( BooleanImage.update( Edge, X-1, Y-1, true );
                    follow( X-1, Y-1 ) )
                else
                  () ;
                if check( X, Y-1 ) then
                  ( BooleanImage.update( Edge, X, Y-1, true );
                    follow( X, Y-1 ) )
                else
                  () ;
                if check( X+1, Y-1 ) then
                  ( BooleanImage.update( Edge, X+1, Y-1, true );
                    follow( X+1, Y-1 ) )
                else
                  () )
            
            val E = BooleanImage.sub( Edge, X, Y )

          in
            if not E andalso M>High then
              ( BooleanImage.update( Edge, X, Y, true ); 
                follow( X, Y ) )
            else
              ()
          end )
        Max

    in
      Edge 
    end

    val findEdges = 
      findEdges' ( Math.sqrt 2.0, highPercentageLowRatio( 0.7, 0.4 ) )
    

  end (* local *)

end (* structure Canny *)
