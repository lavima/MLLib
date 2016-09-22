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

  fun createGaussian( sigma : real ) : image = 
  let 
    val size = Real.ceil( sigma*8.0 ) + 1
    val filter = RealGrayscaleImage.zeroImage( 1, size )
    val _ = 
      RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor 
        ( fn( _, x, _ ) => 
          let
            val xOverSigma = real( x-( size div 2 ) )/sigma
          in
            Math.exp(~0.5*( xOverSigma*xOverSigma ) )
          end )
        ( RealGrayscaleImage.full filter )
    val _ = ImageUtil.normalizeSumReal filter
  in
    filter 
  end

  fun build( im : image ) : edge list =
  let
    val ( height, width ) = RealGrayscaleImage.dimensions im

    fun diff( m : real, x : int, y : int ) : real = 
      Real.abs( m-sub( im, y, x ) )
  in
    foldi 
      ( fn( y, x, m, edges ) =>
          if x<width-1 andalso y<height-1 andalso y>0 then
            ( x+y*width, x+1+( y-1 )*width, diff( m, x+1, y-1 ) )::
            ( x+y*width, x+1+y*width, diff( m, x+1, y ) )::
            ( x+y*width, x+1+( y+1 )*width, diff( m, x+1, y+1 ) )::
            ( x+y*width, x+( y+1 )*width, diff( m, x, y+1 ) )::edges
          else if x<width-1 andalso y<height-1 then
            ( x+y*width, x+1+y*width, diff( m, x+1, y ) )::
            ( x+y*width, x+1+( y+1 )*width, diff( m, x+1, y+1 ) )::
            ( x+y*width, x+( y+1 )*width, diff( m, x, y+1 ) )::edges
          else if x<width-1 andalso y>0 then
            ( x+y*width, x+1+( y-1 )*width, diff( m, x+1, y-1 ) )::
            ( x+y*width, x+1+y*width, diff( m, x+1, y ) )::edges
          else if x<width-1 then
            ( x+y*width, x+1+y*width, diff( m, x+1, y ) )::edges
          else if y<height-1 then
            ( x+y*width, x+( y+1 )*width, diff( m, x, y+1 ) )::edges
          else
            edges )
      []
      ( RealGrayscaleImage.full im )
  end

  fun segment( im : image, sigma : real, c : real, min : real ) 
      : IntGrayscaleImage.image = 
  let
    val ( height, width ) = RealGrayscaleImage.dimensions im

    val gaussian = createGaussian sigma 
    val smooth = convolve( convolve( im, gaussian ), transposed gaussian )

    val edges = sort( Array.fromList( build smooth ) )

    val ds = DisjointSet.init( width*height, c )
    val _ = 
      Array.app
        ( fn( ( f, t, d ) ) =>
          let
            val ( i1, _, _, t1 ) = DisjointSet.find( ds, f ) 
            val ( i2, _, _, t2 ) = DisjointSet.find( ds, t ) 
          in
            if not( i1=i2 ) andalso ( d<=t1 andalso d<=t2 ) then (
              DisjointSet.union( ds, i1, i2 );
              DisjointSet.update( ds, i1, fn( _, _, s, _ ) => d+c/s ) ) 
            else
              () 
            end )
        edges

    val _ = 
      Array.app
        ( fn( ( f, t, d ) ) => 
          let
            val ( i1, _, s1, _ ) = DisjointSet.find( ds, f ) 
            val ( i2, _, s2, _ ) = DisjointSet.find( ds, t ) 
          in
            if not( i1=i2 ) andalso ( s1<min orelse s2<min ) then
              DisjointSet.union( ds, i1, i2 )
            else 
              ()
          end )
        edges

    val out = IntGrayscaleImage.zeroImage( height, width )   
    val _ = 
      IntGrayscaleImage.modifyi IntGrayscaleImage.RowMajor
        ( fn( y, x, _ ) => #1( DisjointSet.find( ds, x+y*width ) ) )
        ( IntGrayscaleImage.full out )
  in
    out
  end

end (* struct FH *)
