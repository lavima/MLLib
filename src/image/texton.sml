(*
* filename: texton.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains generation of filters and textons as used in gPb
*)


structure Texton =
struct

    (*
     * Creates the filter bank used in gPb using the provided number
     * of orientations for edge filters.
     *)
    fun createTextonFilters( nori : int, sigma : real ) 
        : RealGrayscaleImage.image list =
    let

      val evenFilters = 
        List.tabulate(
          nori, 
          fn i => 
            FilterUtil.createGaussianMaskGPB2D 2 (
              sigma,
              sigma * 3.0, 
              sigma * 3.0,
              3.0, 
              false, 
              ~( real i*Math.pi/real nori ) ) )

       val oddFilters = 
        List.tabulate(
          nori, 
          fn i => 
            FilterUtil.createGaussianMaskGPB2D 2 (
              sigma,
              sigma * 3.0, 
              sigma * 3.0,  
              3.0, 
              true, 
              ~( real i*Math.pi/real nori) ) )
       
       val csFilter = 
        FilterUtil.createGausCenterSurround( sigma, Math.sqrt 3.0 )
    
    in 
      evenFilters @ oddFilters @ [ csFilter ]
    end

    fun generateTextons( image : RealGrayscaleImage.image, 
                         nori : int, 
                         sigma : real,
                         k: int,
                         maxIterations: int ) 
        : IntGrayscaleImage.image =
    let
      val ( height, width ) = RealGrayscaleImage.dimensions image

      val filters = createTextonFilters(nori, sigma);
      val convolveFun = 
        RealGrayscaleImage.convolve
          ( RealGrayscaleImage.ZeroExtension, RealGrayscaleImage.OriginalSize )

      val responses = 
        Array.fromList(
          List.foldl 
            ( fn( x : RealGrayscaleImage.image, a ) => 
                ( convolveFun( image, x ) )::a ) 
            [] 
            filters )

      val responseVectors = 
        List.tabulate( width*height,
          fn i => 
            List.tabulate(
              List.length filters, 
              fn j => 
                RealGrayscaleImage.sub( 
                  Array.sub( responses, j ), 
                  i div width,
                  i mod width ) ) )

      val assignments = Array.fromList( #1( 
        KMeans.cluster(
          k, 
          List.length filters, 
          responseVectors, 
          maxIterations ) ) )

      val textonImage = IntGrayscaleImage.tabulate IntGrayscaleImage.RowMajor
        ( height, width, ( fn( i, j ) => Array.sub( assignments, i*width+j ) ) )
    in
      textonImage
    end  

end
