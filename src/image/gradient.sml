(*
* file: gradient.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for generating gradients from images.
* 
*)


structure Gradient =
struct
  
  fun gradientQuantized( image : IntGrayscaleImage.image, 
                         bins : int, 
                         radius : int,
                         histSmoothSigma : real )
    : RealGrayscaleImage.image =
  let
    val ( height, width ) = IntGrayscaleImage.dimensions image

    val smoothKernel = 
      if Real.==( histSmoothSigma, 0.0 ) then 
        RealGrayscaleImage.fromList [ [ 0.0 ] ]
      else
        FilterUtil.createGaussianMaskgPb 0
        ( histSmoothSigma*(real bins), Real.ceil( histSmoothSigma*3.0 ) )

    fun generateIntegralImage( bin : int ) =
      IntSumAreaTable.buildTable
        ( height, 
          width, 
          fn ( i, j ) => if Array2.sub( image, i, j )=bin then 1 else 0 )

    val intImages = List.tabulate( bins, generateIntegralImage )

    fun hist(i, j, height, width) =
    let
       val counts = List.foldr 
          (fn (x, a) => ( IntSumAreaTable.sum x ( i, j, height, width ) ) :: a )          [] 
          intImages

       val sum = List.foldl Int.+ 0 counts
    in
      (sum, counts)
    end

    val border = radius*2

    fun gradient(i : int, j : int) : real =
      if i<border orelse j<border orelse 
         i>(height-border-1) orelse j>(width-border-1) then
        0.0
      else
      let
        
        val (tsum, top) = hist( i-radius, j-radius, radius, 2*radius )
        val (bsum, bot) = hist( i, j-radius, radius, 2*radius )

        val normTop = List.foldl 
          ( fn( x, a ) => (real x / real tsum)::a ) [] top 
        val normBot = List.foldl 
          ( fn (x, a) => ((real x) / (real bsum)) :: a ) [] bot

        val topHist = RealGrayscaleImage.fromList [ normTop ]
        val botHist = RealGrayscaleImage.fromList [ normBot ]
   
        fun smooth(hist) = RealGrayscaleImage.convolve 
          ( RealGrayscaleImage.ZeroExtension, RealGrayscaleImage.FullSize )
          ( hist, smoothKernel )
        
        val ( smoothedTop, smoothedBot ) = 
          if Real.==( histSmoothSigma, 0.0 ) then ( topHist, botHist )
          else ( smooth topHist, smooth botHist )

        fun element(t, b, a) = a+( if (t+b<0.000000001) then 0.0
                                   else ( Math.pow(t-b, 2.0))/(t+b) )
        
        val sum = RealGrayscaleImage.foldi RealGrayscaleImage.RowMajor
                  ( fn ( i, j, x, a ) =>
                    element(x, RealGrayscaleImage.sub( smoothedBot, i, j ), a) )
                  ( 0.0 )
                  ( RealGrayscaleImage.full smoothedTop )
         
      in
        0.5 * sum 
      end
      
      val gradient = 
        RealGrayscaleImage.tabulate RealGrayscaleImage.RowMajor 
          ( height, width, gradient)
    in
      gradient
    end

  fun gradient( image : RealGrayscaleImage.image, 
                bins : int,
                radius : int,
                histSmoothSigma : real ) =
    gradientQuantized
      ( ImageUtil.quantizeImage( image, bins ), bins, radius, histSmoothSigma )

  local

    fun postProcessGradient ( ( height, width ) : int * int,
                              image : RealGrayscaleImage.image, 
                              ori : real,
                              ( savMaj, savMin ) : real * real ) 
      : RealGrayscaleImage.image =
    let
      val grad = RealGrayscaleImage.rotateCrop( image, ~ori, height, width )
      val grad = FilterUtil.savgol( grad, savMaj, savMin, ori+Math.pi/2.0 )
    in
      grad
    end

  in
    fun orientedGradientReal( image : RealGrayscaleImage.image, 
                              bins : int,
                              radius : int,
                              ori : real,
                              savgol : real * real,
                              histSmoothSigma : real ) =
    let
      val dim = RealGrayscaleImage.dimensions image
      val rotated = RealGrayscaleImage.rotate( image, ori )
      val gradient = gradient( rotated, bins, radius, histSmoothSigma )
    in
      postProcessGradient( dim, gradient, ori, savgol )
    end

    fun orientedGradientInt( image : IntGrayscaleImage.image,
                             bins : int,
                             radius : int,
                             ori : real,
                             savgol : real * real,
                             histSmoothSigma : real ) =
    let
      val dim = IntGrayscaleImage.dimensions image
      val rotated = IntGrayscaleImage.rotate( image, ori )
      val gradient = gradientQuantized( rotated, bins, radius, histSmoothSigma )
    in
      postProcessGradient( dim, gradient, ori, savgol )      
    end
  end

end

