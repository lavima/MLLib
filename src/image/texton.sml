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
    fun createTextonFilters(nori : int,
                            sigma : real) : 
           GrayscaleImageReal.image list =
    let

       val evenFilters = List.tabulate(nori, 
             fn(i) => FilterUtil.createGaussianMaskGPB2D 2 (sigma,
                   sigma * 3.0, sigma * 3.0,
                   3.0, false, ~((real i) * Math.pi / (real nori))))

       val oddFilters =  List.tabulate(nori, 
             fn(i) => FilterUtil.createGaussianMaskGPB2D 2 (sigma,
                  sigma * 3.0, sigma * 3.0,  
                  3.0, true, ~((real i) * Math.pi / (real nori))))
       
       val csFilter = FilterUtil.createGausCenterSurround(sigma, Math.sqrt 3.0)
    
    in 
       evenFilters @ oddFilters @ [csFilter]
    end

    fun generateTextons
       (image : GrayscaleImageReal.image, 
        nori : int, 
        sigma : real,
        k: int,
        maxIterations: int) : 
         GrayscaleImageReal.image =
    let
       val {width=width, height=height,...} = image

       val filters = createTextonFilters(nori, sigma);
       val convolveFun = GrayscaleImageReal.convolve
          (ImageCommon.zero, ImageCommon.original)

       val responses = List.foldl 
          (fn (x : GrayscaleImageReal.image, a) => 
              (convolveFun (image, x))::a ) [] filters

       fun buildVector(i : int) : real list =
             List.tabulate(List.length(filters), 
               fn(j) => (GrayscaleImageReal.sub'(List.nth (responses, j) , i)))
          

       val responseVectors = List.tabulate(
           width * height, buildVector)
    

       val (assignments, _)  = 
         KMeans.cluster(k, List.length(filters), responseVectors, maxIterations)

       val responseImage = GrayscaleImageReal.zeroImage(width, height)
       val _ = GrayscaleImageReal.modifyi (fn (i, x) =>
                    real (List.nth(assignments, i))) responseImage
    in
       responseImage
    end  


end
