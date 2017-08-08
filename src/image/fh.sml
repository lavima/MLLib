(*
* filename: fh.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a SML implementation of the graph-based image segmentation
* algorithm introduced in "Efficient graph-based image segmentation" by
* Felzenszwalb & Huttenlocher.
*)

signature FH =
sig
  type image
  type segmap

  val segment : real * real * int -> image -> segmap
end

signature FH_SPEC =
sig
  type image 
  type pixel

  val sub : image * int * int -> pixel
  val convolve : image * image -> image
  val transposed : image -> image
  val dimensions : image -> int * int

  val createGaussian : real -> image

  val diff : image * pixel * int * int -> real
end

structure FHUtil =
struct

  fun createGaussian( sigma : real ) : RealGrayscaleImage.image = 
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

  fun createGaussianRGB( sigma : real ) : RealRGBImage.image =
  let
    val filter = createGaussian sigma
    val ( height, width ) = RealGrayscaleImage.dimensions filter
    val out = RealRGBImage.zeroImage( height, width ) 
    val _ = 
      RealRGBImage.modifyi RealRGBImage.RowMajor
        ( fn( y, x, _ ) => 
          let
            val v = RealGrayscaleImage.sub( filter, y, x )
          in
            ( v, v, v )
          end )
        ( RealRGBImage.full out )
  in
    out
  end

end (* FHUtil *)

functor FHFun( Spec : FH_SPEC ) : FH = 
struct

  type image = Spec.image

  val sub = Spec.sub
  val convolve = Spec.convolve
  val transposed = Spec.transposed
  val dimensions = Spec.dimensions

  val createGaussian = Spec.createGaussian
  val diff = Spec.diff

  type segmap = IntGrayscaleImage.image
  type edge = int * int * real

  (* val sort = 
    ArraySort.quick ( fn( ( _, _, d1 ), ( _, _, d2 ) ) => d1<d2 ) *)
  val sort = 
    ArrayQSort.sort 
      ( fn( ( _, _, d1 ), ( _, _, d2 ) ) => Real.compare( d1, d2 ) )

  fun build( im : image ) : edge list =
  let
    val ( height, width ) = dimensions im

    fun build'( y, x ) =
      case y<height of
        false => []
      | true =>
          case x<width of 
            false => build'( y+1, 0 )
          | true =>
            let
              val pixel = sub( im, y, x )
            in
              if x<width-1 andalso y<height-1 andalso y>0 then
                ( x+y*width, x+1+( y-1 )*width, diff( im, pixel, x+1, y-1 ) )::
                ( x+y*width, x+1+y*width, diff( im, pixel, x+1, y ) )::
                ( x+y*width, x+1+( y+1 )*width, diff( im, pixel, x+1, y+1 ) )::
                ( x+y*width, x+( y+1 )*width, diff( im, pixel, x, y+1 ) )::
                build'( y, x+1 )
              else if x<width-1 andalso y<height-1 then
                ( x+y*width, x+1+y*width, diff( im, pixel, x+1, y ) )::
                ( x+y*width, x+1+( y+1 )*width, diff( im, pixel, x+1, y+1 ) )::
                ( x+y*width, x+( y+1 )*width, diff( im, pixel, x, y+1 ) )::
                build'( y, x+1 )
              else if x<width-1 andalso y>0 then
                ( x+y*width, x+1+( y-1 )*width, diff( im, pixel, x+1, y-1 ) )::
                ( x+y*width, x+1+y*width, diff( im, pixel, x+1, y ) )::
                build'( y, x+1 )
              else if x<width-1 then
                ( x+y*width, x+1+y*width, diff( im, pixel, x+1, y ) )::
                build'( y, x+1 )
              else if y<height-1 then
                ( x+y*width, x+( y+1 )*width, diff( im, pixel, x, y+1 ) )::
                build'( y, x+1 )
              else
                build'( y, x+1 )
            end
  in
    build'( 0, 0 )
  end

  fun segment( sigma : real, c : real, min : int ) ( im : image ) : segmap = 
  let
    val ( height, width ) = dimensions im

    val gaussian = createGaussian sigma 
    val smooth = convolve( convolve( im, gaussian ), transposed gaussian )

    val graph = build smooth
    val graphArr = Array.fromList graph
    val _ = sort( graphArr )

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
              DisjointSet.update( ds, i1, fn( _, _, s, _ ) => d+c/real s ) ) 
            else
              () 
            end )
        graphArr

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
        graphArr

    val out = IntGrayscaleImage.zeroImage( height, width )   
    val _ = 
      IntGrayscaleImage.modifyi IntGrayscaleImage.RowMajor
        ( fn( y, x, _ ) => #1( DisjointSet.find( ds, x+y*width ) ) )
        ( IntGrayscaleImage.full out )
  in
    out
  end

end (* functor FHFun *)

local

  structure RealGrayscaleSpec =
  struct
    type image = RealGrayscaleImage.image
    type pixel = RealGrayscaleImage.pixel

    val sub = RealGrayscaleImage.sub 
    val convolve = 
      RealGrayscaleImage.convolve 
        ( RealGrayscaleImage.CopyExtension, RealGrayscaleImage.OriginalSize )
    val transposed = RealGrayscaleImage.transposed
    val dimensions = RealGrayscaleImage.dimensions

    val createGaussian = FHUtil.createGaussian 

    fun diff( im : image, m : pixel, x : int, y : int ) : real = 
      Real.abs( m-sub( im, y, x ) )

  end

  structure RealRGBSpec =
  struct
    type image = RealRGBImage.image
    type pixel = RealRGBImage.pixel

    val sub = RealRGBImage.sub 
    val convolve = 
      RealRGBImage.convolve 
        ( RealRGBImage.CopyExtension, RealRGBImage.OriginalSize )
    val transposed = RealRGBImage.transposed
    val dimensions = RealRGBImage.dimensions

    val createGaussian = FHUtil.createGaussianRGB

    fun diff( im : image, ( r1, g1, b1 ) : pixel, x : int, y : int ) : real = 
    let
      val ( r2, g2, b2 ) = sub( im, y, x )
      val rd = r2-r1 
      val bd = b2-b1 
      val gd = g2-g1 
    in
      Math.sqrt( rd*rd + bd*bd + gd*gd )
    end

  end

in
  structure RealGrayscaleFH = FHFun( RealGrayscaleSpec )
  structure RealRGBFH = FHFun( RealRGBSpec )
end
