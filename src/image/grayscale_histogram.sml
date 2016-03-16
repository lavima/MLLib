(*
* file: grayscale_histogram.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures for finding histograms for grayscale images
*)

local 

  structure Word8Image : HISTOGRAM_IMAGE =
  struct

    type image = Word8GrayscaleImage.image
    type pixel = Word8GrayscaleImage.pixel
    
    val app = Word8GrayscaleImage.app Word8GrayscaleImage.RowMajor
    val less = Word8.<

    fun fromReal x = Word8.fromInt( Real.trunc( x*255.0 ) )

  end 

  structure RealImage : HISTOGRAM_IMAGE =
  struct

    type image = RealGrayscaleImage.image
    type pixel = RealGrayscaleImage.pixel
    
    val app = RealGrayscaleImage.app RealGrayscaleImage.RowMajor
    val less = Real.<

    fun fromReal x = x

  end 

  structure IntImage : HISTOGRAM_IMAGE =
  struct

    type image = IntGrayscaleImage.image
    type pixel = IntGrayscaleImage.pixel
    
    val app = IntGrayscaleImage.app IntGrayscaleImage.RowMajor
    val less = Int.<

    fun fromReal x = Real.trunc( x*255.0 )

  end

in
  structure Word8GrayscaleHistogram = HistogramFun( Word8Image ) 
  structure IntGrayscaleHistogram = HistogramFun( IntImage ) 
  structure RealGrayscaleHistogram = HistogramFun( RealImage ) 
end
