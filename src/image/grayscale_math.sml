(*
* file: grayscale_math.sml
* author: Marius Geitle <marius.geitle@hiof.no>
*
* This file contains functionality for basic mathematical operations on
* grayscale images.
*)

structure GrayscaleMath =
struct

  fun maxReal( im : RealGrayscaleImage.image ) : real =
    RealGrayscaleImage.fold RealGrayscaleImage.RowMajor 
        Real.max
        Real.negInf 
        im


  fun minReal( im : RealGrayscaleImage.image ) : real =
    RealGrayscaleImage.fold RealGrayscaleImage.RowMajor 
        Real.min 
        Real.posInf 
        im

  fun sumReal( im : RealGrayscaleImage.image ) : real =
    RealGrayscaleImage.fold RealGrayscaleImage.RowMajor 
        Real.+
        0.0 
        im

  fun meanReal( im : RealGrayscaleImage.image ) : real =
  let
    val ( height, width ) = RealGrayscaleImage.dimensions im
    val sum = sumReal im
  in
    sum / ( real( width*height ) )
  end

  fun maxInt( im : IntGrayscaleImage.image ) : int =
    IntGrayscaleImage.fold IntGrayscaleImage.RowMajor 
        ( Int.max )
        ( Option.valOf Int.minInt )
        ( im )


  fun minInt( im : IntGrayscaleImage.image ) : int =
    IntGrayscaleImage.fold IntGrayscaleImage.RowMajor 
        ( Int.min )
        ( Option.valOf Int.maxInt )
        ( im )

  fun sumInt( im : IntGrayscaleImage.image ) : int =
    IntGrayscaleImage.fold IntGrayscaleImage.RowMajor 
        Int.+
        0
        im


end
