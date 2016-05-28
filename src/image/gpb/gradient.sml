(*
* file: gradient.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for generating gradients from images.
* 
*)

signature GRADIENT =
sig
  val gradientQuantized 
    : IntGrayscaleImage.image * int * int * int * ( real * real ) * real option 
    -> RealGrayscaleImage.image list

  val gradientReal 
    : RealGrayscaleImage.image * int * int * int * ( real * real ) * real option
    -> RealGrayscaleImage.image list
end



