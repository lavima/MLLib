(*
* filename: filter_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with various filter utilities.
*)


structure FilterUtil =
struct

  (*
  * Create the vertical (X) and horizontal (Y) sobel masks.
  *)
  fun createSobelMasks() : GrayscaleImageReal.image * GrayscaleImageReal.image =
  let
    val XMask = [ 1.0, 0.0, ~1.0, 2.0, 0.0, ~2.0, 1.0, 0.0, ~1.0 ]
    val YMask = [ 1.0, 2.0, 1.0, 0.0, 0.0, 0.0, ~1.0, ~2.0, ~1.0 ]
  in
    ( GrayscaleImageReal.fromList( 3, 3, XMask ), 
      GrayscaleImageReal.fromList( 3, 3, YMask ) ) 
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
  fun createGaussianMask( Sigma : real ) : GrayscaleImageReal.image =
  let
    val Length = 8.0*Real.realCeil Sigma 
    val N = ( Length-1.0 )/2.0
    val Xs = ListUtil.fromToReal( ~N, N )

    val Mask = GrayscaleImageReal.zeroImage( List.length Xs, 1 )

    val C = 1.0/( Math.sqrt( 2.0*Math.pi )*Sigma )

    fun gaussian( X : real ) : real = 
      C*Math.exp( ~( Math.pow( X, 2.0 )/( 2.0*Math.pow( Sigma, 2.0 ) ) ) ) 

    val _ = 
      List.foldl
        ( fn( X, I ) =>
          let
            val _ = GrayscaleImageReal.update'( Mask, I, gaussian X ) 
          in
            I+1
          end )
        0
        Xs

    val Sum = GrayscaleImageReal.foldl ( fn( X, S ) => S+X ) 0.0 Mask
    val _ = GrayscaleImageReal.modify ( fn( X ) => X/Sum ) Mask

  in
    Mask
  end

  (*
   * Create a one dimensional gaussian mask of the required 
   * derivitive based on the specified standard deviation and derivation.
   *
   * This function has beeen implemented according to how a gaussian filter
   * is implemented in the gPb implementation.
   *)

  fun createGaussianMaskgPb (derivitive : int) ( sigma : real, support : int ) 
    : GrayscaleImageReal.image =
  let
    val size = 2 * support + 1;

    val sigma2_inv = 1.0 / (sigma * sigma);
    val neg_two_sigma2_inv = sigma2_inv * ~0.5;
    
    val elemFunction = case derivitive of
       0 => (fn(x : real) => Math.exp(x * x * neg_two_sigma2_inv))
     | 1 => (fn(x : real) => Math.exp(x * x * neg_two_sigma2_inv) * ~x)
     | 2 => (fn(x : real) => 
          Math.exp(x * x * neg_two_sigma2_inv) * (x * x * sigma2_inv - 1.0));
    
    val mask = GrayscaleImageReal.zeroImage(size, 1);
    
    val _ = GrayscaleImageReal.modifyxy( 
              fn(x, y, z) => elemFunction(Real.fromInt(x - support))
              ) mask;
  in
     mask
  end

 fun createGaussianMaskGPB2D (deri : int)
      (sigma : real,
       supportX : real, 
       supportY : real, 
       elongation : real, 
       hilbert : bool, 
       ori : real) : GrayscaleImageReal.image =
 let
   val sigmaY = sigma 
   val sigmaX = sigma / elongation

   val corSupportX = Real.ceil(Real.max(
    abs(supportX * Math.cos(ori) - supportY * Math.sin(ori)),
    abs(supportX * Math.cos(ori) + supportY * Math.sin(ori))));
   val corSupportY = Real.ceil(Real.max(
    abs(supportX * Math.sin(ori) - supportY * Math.cos(ori)),
    abs(supportX * Math.sin(ori) + supportY * Math.cos(ori))));

   val gausX = createGaussianMaskgPb deri (sigmaX, corSupportX)
   val gausY = createGaussianMaskgPb 0 (sigmaY, corSupportY)

   val _ = if (hilbert) then
    let
       val hilbert = SignalUtil.hilbert(#Values(gausX))
    in
       GrayscaleImageReal.modifyi 
          (fn (i, _) => Array.sub (hilbert, i)) gausX
    end
    else ()

   val filter = GrayscaleImageReal.tabulatexy 
       (#Width(gausX), #Width(gausY), 
        fn (x, y) => GrayscaleImageReal.sub(gausX, x, 0) * 
                   GrayscaleImageReal.sub(gausY, y, 0));
  
   val rotated = GrayscaleImageReal.rotateCrop
      (filter, ori, 
      Real.ceil(2.0 * supportX + 1.0), 
      Real.ceil(2.0 * supportY + 1.0));

    val _ = if deri > 0 then
       ImageUtil.MakeRealZeroMean' rotated
    else ()

    val _ = ImageUtil.MakeRealL1Norm' rotated

  in
    rotated
  end

  fun createGausCenterSurround(sigma : real, scaleFactor : real) = 
  let
     val sigmaInner = sigma / scaleFactor;

     val outer = createGaussianMaskGPB2D 0 (sigma,
       sigma * 3.0, sigma * 3.0, 1.0, false, 0.0);

     val inner =  createGaussianMaskGPB2D 0 (sigmaInner,
       sigma * 3.0, sigma * 3.0, 1.0, false, 0.0);

     val image = GrayscaleImageReal.subtract(outer, inner)

     val _ = ImageUtil.MakeRealZeroMean' image
     val _ = ImageUtil.MakeRealL1Norm' image
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
  fun createGaussianMask2( Sigma : real ) : GrayscaleImageReal.image =
  let
    val MaskSize = Real.ceil( Sigma*8.0 )+( Real.ceil( Sigma*8.0 )+1 ) mod 2
    val MaskCenter = MaskSize div 2
    val Mask = GrayscaleImageReal.zeroImage( MaskSize, MaskSize )

    fun gaussian( X : int, Y : int ) : real = 
      Math.exp( ~0.5*( ( 
        Math.pow( Real.fromInt X, 2.0 ) + Math.pow( Real.fromInt Y, 2.0 ) ) / 
        Math.pow( Sigma, 2.0 ) ) ) 

    val _ = 
      GrayscaleImageReal.modifyi
        ( fn( I, _ ) =>
            gaussian( I mod MaskSize-MaskCenter, I div MaskSize-MaskCenter ) )
        Mask

    val Sum = GrayscaleImageReal.foldl ( fn( X, S ) => S+X ) 0.0 Mask
    val _ = GrayscaleImageReal.modify ( fn( X ) => X/Sum ) Mask

  in
    Mask
  end

end (* structure FilterUtil *)
