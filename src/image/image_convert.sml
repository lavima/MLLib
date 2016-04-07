(*
* file: image_convert.sml
* author: Lars Vidar Magnusson
*
* This file contains a structure with functions for converting between image
* types.
*)

structure ImageConvert =
struct

  (* 
  * Convert RGB to CIELab. The implementation have been copied from the
  * gPb source code.
  *)
  fun realRGBToCIELab( im : RealRGBImage.image ) : RealCIELabImage.image =
  let
    val ( height, width ) = RealRGBImage.dimensions im

    val out = RealCIELabImage.zeroImage( height, width ) 
    val _ =
      RealRGBImage.appi RealRGBImage.RowMajor
        ( fn( i, j, ( r, g, b ) ) => 
          let
            val true = 
              r>=0.0 andalso r<=1.0 andalso 
              g>=0.0 andalso g<=1.0 andalso 
              b>=0.0 andalso b<=1.0 

            (* RGB -> XYZ *)

            val x = 0.412453*r + 0.357580*g + 0.180423*b
            val y = 0.212671*r + 0.715160*g + 0.072169*b
            val z = 0.019334*r + 0.119193*g + 0.950227*b

            (* XYZ of D65 reference white *)

            val xn = 0.950456
            val yn = 1.0
            val zn = 1.088754

            (* XYZ -> 1976 CIELab *)
            
            val rx = x/xn
            val ry = y/yn
            val rz = z/zn

            val thresh = 0.008856

            fun f( t : real ) : real =
              if t>thresh then
                Math.pow( t, 1.0/3.0 )
              else
                7.787*t + 16.0/116.0

            val fx = f(rx)
            val fy = f(ry)
            val fz = f(rz)

            val l = 
              if ry>thresh then
                116.0*Math.pow( ry, 1.0/3.0 )-16.0
              else
                903.3*ry
            val a = 500.0*( fx-fy )
            val b = 200.0*( fy-fz )

            val true =
              l>=0.0 andalso l<=100.0 andalso
              a>= ~120.0 andalso a<=120.0 andalso
              b>= ~120.0 andalso b<=120.0 
          in
            RealCIELabImage.update( out, i, j, ( l, a, b ) )
          end )
        ( RealRGBImage.full im )
  in
    out
  end

  fun realRGBtoGray( im : RealRGBImage.image ) : RealGrayscaleImage.image =
  let
    val ( height, width ) = RealRGBImage.dimensions im

    val out = RealGrayscaleImage.zeroImage( height, width ) 

    val _ = RealRGBImage.appi RealRGBImage.RowMajor
        ( fn( i, j, ( r, g, b ) ) => 
          let
            val grayValue = r*0.29894+g*0.58704+b*0.11402
          in
            RealGrayscaleImage.update( out, i, j, grayValue )
          end )
        ( RealRGBImage.full im )
  in
    out
  end

  fun realGrayscaleIntToGrayscaleReal( im : IntGrayscaleImage.image )
    : RealGrayscaleImage.image =
  let
    val ( height, width ) = IntGrayscaleImage.dimensions im  
  in
    RealGrayscaleImage.tabulate RealGrayscaleImage.RowMajor
    ( height, width,  
      fn ( y, x ) => ( real ( IntGrayscaleImage.sub( im, y, x ) ) ) / 255.0 )
  end


end (* structure ImageConvert *)
