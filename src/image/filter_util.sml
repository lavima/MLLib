(*
* filename: filter_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with various filter utilities.
*)


structure FilterUtil =
struct

  (*
  * Create the vertical and horizontal sobel masks.
  *)
  fun createSobelMasks() : RealGrayscaleImage.image * RealGrayscaleImage.image =
  let
    val xMask = [ 1.0, 0.0, ~1.0, 2.0, 0.0, ~2.0, 1.0, 0.0, ~1.0 ]
    val yMask = [ 1.0, 2.0, 1.0, 0.0, 0.0, 0.0, ~1.0, ~2.0, ~1.0 ]
  in
    ( RealGrayscaleImage.fromList'( 3, 3, xMask ), 
      RealGrayscaleImage.fromList'( 3, 3, yMask ) ) 
  end

  (* 
  * Create a gaussian mask based on the specified standard deviation. The mask
  * is normalized to sum to 1.0.
  *
  * This function has been implemented according to how a gaussian filter mask 
  * is created in the Canny implementation in Matlab.
  *
  * Note that the mask is one-dimensional in that the height of the mask is 1 
  * i.e. the mask must be applied in each direction.
  *)
  fun createGaussianMask( sigma : real ) : RealGrayscaleImage.image =
  let
    val l = 8.0*Real.realCeil sigma 
    val n = ( l-1.0 )/2.0
    val xs = ListUtil.fromToReal( ~n, n )

    val mask = RealGrayscaleImage.zeroImage( 1, List.length xs )

    val c = 1.0/( Math.sqrt( 2.0*Math.pi )*sigma )

    fun gaussian( x : real ) : real = 
      c*Math.exp( ~( Math.pow( x, 2.0 )/( 2.0*Math.pow( sigma, 2.0 ) ) ) ) 

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

  (*
   * Create a one dimensional gaussian mask of the required 
   * derivitive based on the specified standard deviation and derivation.
   *
   * This function has beeen implemented according to how a gaussian filter
   * is implemented in the gPb implementation.
   *)
  fun createGaussianMaskgPb (derivitive : int) ( sigma : real, support : int ) 
    : RealGrayscaleImage.image =
  let
    val size = 2*support+1;

    val sigma2_inv = 1.0/(sigma*sigma);
    val neg_two_sigma2_inv = sigma2_inv* ~0.5;
    
    val elemFunction = 
      case derivitive of
        0 => ( fn( x : real ) => Math.exp( x*x*neg_two_sigma2_inv ) )
      | 1 => ( fn( x : real ) => Math.exp( x*x*neg_two_sigma2_inv )* ~x )
      | 2 => 
          ( fn( x : real ) => 
              Math.exp( x*x*neg_two_sigma2_inv )*(x*x*sigma2_inv-1.0 ) )
    
    val mask = RealGrayscaleImage.zeroImage( 1, size )
    
    val _ = 
      RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor 
        ( fn( _, x, _ ) => elemFunction( real( x-support ) ) ) 
        ( RealGrayscaleImage.full mask )

     val _ = 
      if derivitive > 0 then
        ImageUtil.makeRealZeroMean' mask
      else 
        ()

    val _ = ImageUtil.makeRealL1Norm' mask
    
  in
     mask
  end

  fun createGaussianMaskGPB2D  ( deri : int )
                               ( sigma : real,
                                 supportX : real,
                                 supportY : real, 
                                 elongation : real,
                                 hilbert : bool,
                                 ori : real ) 
    : RealGrayscaleImage.image =
  let
    val sigmaY = sigma 
    val sigmaX = sigma/elongation

    val corSupportX = Real.ceil(Real.max(
      abs(supportX * Math.cos(ori) - supportY * Math.sin(ori)),
      abs(supportX * Math.cos(ori) + supportY * Math.sin(ori))));
    val corSupportY = Real.ceil(Real.max(
      abs(supportX * Math.sin(ori) - supportY * Math.cos(ori)),
      abs(supportX * Math.sin(ori) + supportY * Math.cos(ori))));

    val gausX = createGaussianMaskgPb deri ( sigmaX, corSupportX )
    val gausY = createGaussianMaskgPb 0 ( sigmaY, corSupportY)

    val _ = 
      if hilbert then
      let
         val hilbert = SignalUtil.hilbert( RealGrayscaleImage.row( gausX, 0 ) )
      in
        RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor
          ( fn ( _, j, _) => Vector.sub( hilbert, j ) ) 
          ( RealGrayscaleImage.full gausX )
      end
      else 
        ()

    val filter = 
      RealGrayscaleImage.tabulate RealGrayscaleImage.RowMajor 
        ( RealGrayscaleImage.nCols gausX, 
          RealGrayscaleImage.nCols gausY, 
          fn( y, x ) => 
            RealGrayscaleImage.sub( gausX, 0, x ) 
            * 
            RealGrayscaleImage.sub( gausY, 0, y ) )
  
    val rotated = 
      RealGrayscaleImage.rotateCrop(
        filter, 
        ori, 
        Real.ceil(2.0 * supportX + 1.0), 
        Real.ceil(2.0 * supportY + 1.0) )

    val _ = 
      if deri > 0 then
        ImageUtil.makeRealZeroMean' rotated
      else 
        ()

    val _ = ImageUtil.makeRealL1Norm' rotated

  in
    rotated
  end

  fun createGausCenterSurround( sigma : real, scaleFactor : real ) = 
  let
    val sigmaInner = sigma/scaleFactor;

    val outer = 
      createGaussianMaskGPB2D 0 
        ( sigma, sigma * 3.0, sigma * 3.0, 1.0, false, 0.0 )

    val inner = 
      createGaussianMaskGPB2D 0 
        ( sigmaInner, sigma * 3.0, sigma * 3.0, 1.0, false, 0.0 )

    val image = RealGrayscaleImage.subtract( outer, inner )

    val _ = ImageUtil.makeRealZeroMean' image
    val _ = ImageUtil.makeRealL1Norm' image
  in
    image
  end


  (* 
  * Create a two dimensional gaussian mask based on the specified standard 
  * deviation.
  * The mask equals in height and width which is always odd in number.
  *
  * TODO Figure out if the Matlab approach taken in the one-dimensional 
  * implementation above should be replicated here.
  *)
  fun createGaussianMask2( sigma : real ) : RealGrayscaleImage.image =
  let
    val maskSize = Real.ceil( sigma*8.0 )+( Real.ceil( sigma*8.0 )+1 ) mod 2
    val maskCenter = maskSize div 2
    val mask = RealGrayscaleImage.zeroImage( maskSize, maskSize )

    fun gaussian( x : int, y : int ) : real = 
      Math.exp( ~0.5*( ( 
        Math.pow( Real.fromInt x, 2.0 ) + Math.pow( Real.fromInt y, 2.0 ) ) / 
        Math.pow( sigma, 2.0 ) ) ) 

    val _ = 
      RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor
        ( fn( i, j, _ ) => gaussian( j, i ) )
        ( RealGrayscaleImage.full mask )

    val sum = 
      RealGrayscaleImage.fold RealGrayscaleImage.RowMajor
        ( fn( x, s ) => s+x ) 
        0.0 
        mask

    val _ = 
      RealGrayscaleImage.modify RealGrayscaleImage.RowMajor 
        ( fn( x ) => x/sum ) 
        mask
  in
    mask
  end

  (* Savitzky-Golay implementation that handles borders 
   * better than using a convolving filter, but the borders
   * will still be biased.
   *)
  fun savgol( image: RealGrayscaleImage.image,
              radiusMajor : real,
              radiusMinor : real,
              theta : real ) 
      : RealGrayscaleImage.image =
  let
    val ( height, width ) = RealGrayscaleImage.dimensions image

    val ira2 = 1.0 / Math.pow(radiusMajor, 2.0)
    val irb2 = 1.0 / Math.pow(radiusMinor, 2.0)
    val wr = Real.floor(Real.max(radiusMajor, radiusMinor))
    
    val sint = Math.sin theta
    val cost = Math.cos theta
    val eps = Math.exp(~300.0)

    fun calculateElement( y : int, x : int ) =
    let
      fun loopY(u : int, v : int, d0 : real, d1 : real, d2 : real, 
                d3 : real, d4 : real, v0 : real, v1 : real, v2 : real) =
      let
        val yi = y + v
        val xi = x + u
        val di = ~(real u) * sint + (real v) * cost
        val ei = (real u) * cost + (real v) * sint
      in
        if v < wr then
          if not((yi < 0) orelse (yi >= height)) then 
          let
            val zi = RealGrayscaleImage.sub(image, yi, xi)
            val di2 = di * di
            val d0 = d0 + 1.0
            val d1 = d1 + di
            val d2 = d2 + di2
            val d3 = d3 + di * di2
            val d4 = d4 + di2 * di2
            val v0 = v0 + zi
            val v1 = v1 + zi * di
            val v2 = v2 + zi * di2
          in
            loopY(u, v + 1, d0, d1, d2, d3, d4, v0, v1, v2)
          end
          else loopY(u, v + 1, d0, d1, d2, d3, d4, v0, v1, v2)
        else (d0, d1, d2, d3, d4, v0, v1, v2)
      end

      fun loopX(u, d0, d1, d2, d3, d4, v0, v1, v2) =
        if u < wr then
          if not((x + u < 0) orelse (x + u >= width)) then
          let
            val (d0, d1, d2, d3, d4, v0, v1, v2) =
              loopY(u, ~wr, d0, d1, d2, d3, d4, v0, v1, v2)
          in
             loopX(u + 1, d0, d1, d2, d3, d4, v0, v1, v2)
          end
          else loopX(u + 1, d0, d1, d2, d3, d4, v0, v1, v2)
        else (d0, d1, d2, d3, d4, v0, v1, v2)

      val (d0, d1, d2, d3, d4, v0, v1, v2) =
           loopX(~wr, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

      val detA = ~d2*d2*d2 + 2.0 * d1*d2*d3 - d0*d3*d3 + d0*d2*d4
    in
      if detA > eps then 
        ((~d3*d3+d2*d4)*v0 + (d2*d3-d1*d4)*v1 + (~d2*d2+d1*d3)*v2) / detA
      else RealGrayscaleImage.sub(image, y, x)
    end

  in
    RealGrayscaleImage.tabulate RealGrayscaleImage.RowMajor 
      ( height, width, calculateElement )
  end

  fun applyGammaCorrection( image : RealGrayscaleImage.image, 
                            gamma : real ) : unit =
    RealGrayscaleImage.modify RealGrayscaleImage.RowMajor 
      ( fn ( p ) => Math.pow( p, gamma ) )
      ( image )

  fun applyGammaCorrectionRealRGB( image : RealRGBImage.image, 
                            gamma : real ) : unit =
    RealRGBImage.modify RealRGBImage.RowMajor 
      ( fn ( r, g, b ) => 
        (Math.pow( r, gamma ), Math.pow( g, gamma ), Math.pow( b, gamma ) ) )
      ( image )
    

end (* structure FilterUtil *)
