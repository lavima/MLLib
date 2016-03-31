(*
* filename: grayscale_threshold.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures for finding threshold values from 
* grayscale images.
*)

local

  structure Word8Image =
  struct
    type image = Word8GrayscaleImage.image
    val histogram = Word8GrayscaleHistogram.histogram'
    val dimensions = Word8GrayscaleImage.dimensions
  end

  structure RealImage =
  struct
    type image = RealGrayscaleImage.image
    val histogram = RealGrayscaleHistogram.histogram'
    val dimensions = RealGrayscaleImage.dimensions
  end

  structure IntImage =
  struct
    type image = IntGrayscaleImage.image
    val histogram = IntGrayscaleHistogram.histogram'
    val dimensions = IntGrayscaleImage.dimensions
  end

in
  structure Word8GrayscaleThreshold = ThresholdFun( Word8Image )
  structure RealGrayscaleThreshold = ThresholdFun( RealImage )
  structure IntGrayscaleThreshold = ThresholdFun( IntImage )
end
