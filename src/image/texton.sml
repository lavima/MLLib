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
    fun createTextonFilters(nori : int, sigma : real) : 
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



end
