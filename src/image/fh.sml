(*
* filename: fh.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a SML implementation of the graph-based image segmentation
* algorithm introduced in "Efficient graph-based image segmentation" by
* Felzenszwalb & Huttenlocher.
*)

structure FH = 
struct

  type image = RealGrayscaleImage.image
  type edge = int * int * real


  val sub = RealGrayscaleImage.sub
  val foldi = RealGrayscaleImage.foldi RealGrayscaleImage.RowMajor
  val convolve = 
    RealGrayscaleImage.convolve 
      ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize ) 
  val transposed = RealGrayscaleImage.transposed

  val sort = 
    ArraySort.quick ( fn( ( _, _, d1 ), ( _, _, d2 ) ) => d1<d2 )

  fun build( im : image ) : edge list =
  let
    val ( height, width ) = RealGrayscaleImage.dimensions im

    fun diff( m : real, x : int, y : int ) : real = 
      Real.abs( m-sub( im, y, x ) )
  in
    foldi 
      ( fn( y, x, m, edges ) =>
          if x<width-1 then
            if y<height-1 then
              if y>0 then
                ( x+y*width, x+1+( y-1 )*width, diff( m, x+1, y-1 ) )::
                ( x+y*width, x+1+y*width, diff( m, x+1, y ) )::
                ( x+y*width, x+1+( y+1 )*width, diff( m, x+1, y+1 ) )::
                ( x+y*width, x+( y+1 )*width, diff( m, x, y+1 ) )::edges
              else
                ( x+y*width, x+1+y*width, diff( m, x+1, y ) )::
                ( x+y*width, x+1+( y+1 )*width, diff( m, x+1, y+1 ) )::
                ( x+y*width, x+( y+1 )*width, diff( m, x, y+1 ) )::edges
            else
              ( x+y*width, x+1+y*width, diff( m, x+1, y ) )::
              ( x+y*width, x+1+( y-1 )*width, diff( m, x+1, y-1 ) )::edges
          else if y<height-1 then
            ( x+y*width, x+( y+1 )*width, diff( m, x, y+1 ) )::edges
          else
            edges )
      []
      ( RealGrayscaleImage.full im )
  end


  fun segment( im : image, sigma : real, c : real ) 
      : IntGrayscaleImage.image = 
  let
    val gaussian = FilterUtil.createGaussianMask sigma 
    val smooth = convolve( convolve( im, gaussian ), transposed gaussian )

    val edges = sort( Array.fromList( build smooth ) )
  in
    IntGrayscaleImage.zeroImage( 0, 0 )   
  end

end (* struct FH *)
