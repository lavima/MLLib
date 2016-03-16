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
        ( fn( x ) => x/sum ) 
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
    val gausY = createGaussianMaskgPb 0 (sigmaY, corSupportY)

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
        ( RealGrayscaleImage.nRows gausX, 
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

end (* structure FilterUtil *)
