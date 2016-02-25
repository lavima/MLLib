(*
* file: grayscale_histogram.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains structures for finding histograms for grayscale images
*)

local 

  structure Word8Image : HISTOGRAM_IMAGE =
  struct

    type image = GrayscaleImageWord8.image
    
    val app = GrayscaleImageWord8.app
    val less = Word8.<

    fun fromReal x = Word8.fromInt( Real.trunc( x*255.0 ) )

  end 

  structure RealImage : HISTOGRAM_IMAGE =
  struct

    type image = GrayscaleImageReal.image
    
    val app = GrayscaleImageReal.app
    val less = Real.<

    fun fromReal x = x

  end 

  structure IntImage : HISTOGRAM_IMAGE =
  struct

    type image = GrayscaleImageInt.image
    
    val app = GrayscaleImageInt.app
    val less = Int.<

    fun fromReal x = Real.trunc( x*255.0 )

  end

in
  structure GrayscaleHistogramWord8 = HistogramFun( Word8Image ) 
  structure GrayscaleHistogramInt = HistogramFun( IntImage ) 
  structure GrayscaleHistogramReal = HistogramFun( RealImage ) 
end
