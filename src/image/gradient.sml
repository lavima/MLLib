(*
* file: gradient.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for generating gradients from images.
* 
*)


structure Gradient =
struct
  fun gradient( image : RealGrayscaleImage.image, 
                bins : int,
                radius : int ) =
  let
    val ( height, width ) = RealGrayscaleImage.dimensions image
    val binWidth = 1.0 / (real bins)
      
    val quantized = Array2.tabulate Array2.RowMajor (
      height, width, 
      fn( y, x ) => 
        Real.round((RealGrayscaleImage.sub( image, y, x ) )/binWidth ) )

    fun countOccurance( i : int, j : int, value : int ) : int = 
      if Array2.sub( quantized, i, j )=value then 
        1 
      else 
        0

    fun generateIntegralImage( bin : int ) =
    let
      val intImage = Array2.array( height, width, 0 )

      fun calculateIntElement( 0, 0, _ ) = countOccurance( 0, 0, bin )
      |   calculateIntElement( i, 0, _ ) = 
            Array2.sub( intImage, i-1, 0 ) + countOccurance( i, 0, bin )
      |   calculateIntElement( 0, j, _ ) = 
            Array2.sub( intImage, 0, j-1 ) + countOccurance( 0, j, bin )
      |   calculateIntElement( i, j, _ ) = 
            Array2.sub( intImage, i, j-1 ) + Array2.sub( intImage, i-1, j ) -
            Array2.sub( intImage, i-1, j-1) + countOccurance( i, j, bin )

      val range = { base=intImage, row=0, col=0, nrows=NONE, ncols=NONE }
      val _ = Array2.modifyi Array2.RowMajor calculateIntElement range
    in
       intImage
    end

    val intImages = List.tabulate( bins, generateIntegralImage )

    fun count(intImage, i, j, height, width) : int =
       Array2.sub(intImage, i, j) + 
       Array2.sub(intImage, i+width, j+width) - 
       Array2.sub(intImage, i+width, j) - 
       Array2.sub(intImage, i, j + width)

    val border =  2 * radius

    fun hist(i, j, height, width) =
    let
       val counts = List.foldr 
          (fn (x, a) => (count(x, i, j, height, width))::a ) [] intImages
       val sum = List.foldl Int.+ 0 counts
    in
      (sum, counts)
    end

    fun gradient(i : int, j : int) : real =
      if i < border orelse j < border orelse 
         i > ( height - border - 1 ) orelse j > ( width - border - 1 ) then
        0.0
      else
      let
        
        val (tsum, top) = hist( i-radius, j-radius, radius, 2*radius )
        val (bsum, bot) = hist( i, j-radius, radius, 2*radius )
        
        val normTop = 
          List.foldl ( fn( x, a ) => (real x/real tsum)::a ) [] top 

        val normBot = List.foldl 
          (fn (x, a) => ((real x) / (real bsum)) :: a) [] bot

        fun element(t, b, a) = a +
          (if (t + b < 0.000000001) then 0.0
          else (Math.pow(t - b, 2.0)) / (t + b))

        val sum = 0.5 * (ListPair.foldl element 0.0 (normTop, normBot))
       
      in
        sum 
      end
      
      val gradient = 
        RealGrayscaleImage.tabulate RealGrayscaleImage.RowMajor 
          ( height, width, gradient)
    in
      gradient
    end
   
    fun orientedGradient( image : RealGrayscaleImage.image, 
                          bins : int,
                          radius : int,
                          ori : real ) =
    let
      val ( height, width ) = RealGrayscaleImage.dimensions image
      val rotated = RealGrayscaleImage.rotate( image, ori )
      val gradientOri = gradient( rotated, bins, radius )
      val gradient = 
        RealGrayscaleImage.rotateCrop( gradientOri, ~ori, height, width )
    in
      gradient
    end
end

