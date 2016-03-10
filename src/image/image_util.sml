(*
* file: image_util.sml
* author: Lars Vidar Magnusson
*
* This file contains a structure with utilitary functions for working with
* images 
*)

structure ImageUtil =
struct
  
  fun convertGrayscaleRealToBoolean( im : RealGrayscaleImage.image )
      : BooleanImage.image = 
  let
    val { width, height, ... } = im

    val out = BooleanImage.image( width, height, false )
    val _ =
      RealGrayscaleImage.appxy
        ( fn( x, y, w ) =>
            if w>0.0 then
              BooleanImage.update( out, x, y, true )
            else
              () )
  in
    out
  end

  fun convertGrayscaleWord8ToReal( im : Word8GrayscaleImage.image )
      : RealGrayscaleImage.image =
  let
    val { width, height, ... } = im

    val out = RealGrayscaleImage.image( width, height, 0.0 )
    val _ = 
      Word8GrayscaleImage.appxy
        ( fn( x, y, w ) =>
            RealGrayscaleImage.update( out, x, y, 
              real( Word8.toInt w )/255.0 ) )
        im
  in
    out
  end

  fun convertBooleanToReal( im : BooleanImage.image )
      : RealGrayscaleImage.image =
  let
    val { width, height, ... } = im

    val out = RealGrayscaleImage.image( width, height, 0.0 )
    val _ = 
      BooleanImage.appxy
        ( fn( x, y, w ) =>
            if w then
              RealGrayscaleImage.update( out, x, y, 1.0 )
            else
              RealGrayscaleImage.update( out, x, y, 0.0 ) )
        im
  in
    out
  end

  fun convertBooleanToTransposedReal( im : BooleanImage.image )
      : RealGrayscaleImage.image =
  let
    val { width, height, ... } = im

    val out = RealGrayscaleImage.image( height, width, 0.0 )
    val _ = 
      BooleanImage.appxy
        ( fn( x, y, w ) =>
            if w then
              RealGrayscaleImage.update( out, y, x, 1.0 )
            else
              RealGrayscaleImage.update( out, y, x, 0.0 ) )
        im
  in
    out
  end

  fun copyBooleanToTransposedReal( src : BooleanImage.image, 
                                   dst : RealGrayscaleImage.image )
      : unit =
    BooleanImage.appxy
      ( fn( x, y, w ) =>
          if w then
            RealGrayscaleImage.update( dst, y, x, 1.0 )
          else
            RealGrayscaleImage.update( dst, y, x, 0.0 ) )
      src

  (*
  * Normalize an image of reals to the interval [0.0 1.0]. Note that all pixels 
  * are assumed to be equal to or larger than 0.0.
  *)
  fun normalizeReal( im : RealGrayscaleImage.image )
      : RealGrayscaleImage.image =
  let

    val { width, height, ... } = im

    val normalized = RealGrayscaleImage.image( width, height, 0.0 )

    val max = RealGrayscaleImage.foldl
      ( fn( x, max ) => 
          if x>max then
            x
          else
            max )
      0.0 
      im

    val _ = 
      if max>0.0 then
        RealGrayscaleImage.appi
          ( fn( I, x ) =>
              RealGrayscaleImage.update'( normalized, I, x/max ) )
          im
      else
        ()
  in
    normalized
  end

  fun normalizeReal'( im : RealGrayscaleImage.image )
      : unit =
  let

    val { width, height, ... } = im

    val max = RealGrayscaleImage.foldl
      ( fn( x, max ) => 
          if x>max then
            x
          else
            max )
      0.0 
      im

    val _ = 
      if max>0.0 then
        RealGrayscaleImage.modifyi
          ( fn( I, x ) => x/max )
          im
      else
        ()
  in
    ()
  end

  (*
   * Normalize an image of reals by translating and scaling.
   *)
  fun normalizeReal''( im : RealGrayscaleImage.image )
       : RealGrayscaleImage.image =
  let

    val { width, height, ... } = im

    val normalized = RealGrayscaleImage.image( width, height, 0.0 )    

    val max = RealGrayscaleImage.foldl Real.max Real.negInf im
    val min = RealGrayscaleImage.foldl Real.min Real.posInf im

    val _ = RealGrayscaleImage.appi
          ( fn( I, x ) =>
            RealGrayscaleImage.update'( normalized, I, (x - min)/(max - min) ) )
          im
  in
    normalized
  end

  (*
   * Normalize an image by making it zero mean and l1 norm
   *)
  fun makeRealZeroMean'( image : RealGrayscaleImage.image )
       : unit =
  let

    val { width=width, height=height, ... } = image

    val mean = (RealGrayscaleImage.foldl 
                   (fn (x, a) => x + a) 0.0 image) / (real (width * height))

  in
     RealGrayscaleImage.modify (fn x => x - mean) image    
  end

  fun makeRealL1Norm'( image : RealGrayscaleImage.image )
       : unit =
  let
    val sum = (RealGrayscaleImage.foldl 
                   (fn (x, a) => Real.abs(x) + a) 0.0 image)
  in
     RealGrayscaleImage.modify (fn x => x / sum) image    
  end

  

  fun gradientXReal( im : RealGrayscaleImage.image )
      : RealGrayscaleImage.image =
  let

    val sub = RealGrayscaleImage.sub
    val { width, height, ... } = im

    val out = RealGrayscaleImage.zeroImage( width, height )
    val _ = 
      RealGrayscaleImage.modifyxy
        ( fn( x, y, _ ) => 
            if x=0 then
              sub( im, x+1, y )-sub( im, x, y )
            else if x=width-1 then
              sub( im, x, y )-sub( im, x-1, y )
            else
              ( sub( im, x+1, y )-sub( im, x-1, y ) )/2.0 )
        out
  in
    out
  end

  fun gradientYReal( im : RealGrayscaleImage.image )
      : RealGrayscaleImage.image =
  let

    val sub = RealGrayscaleImage.sub
    val { width, height, ... } = im

    val out = RealGrayscaleImage.zeroImage( width, height )
    val _ = 
      RealGrayscaleImage.modifyxy
        ( fn( x, y, _ ) => 
            if y=0 then
              sub( im, x, y+1 )-sub( im, x, y )
            else if x=width-1 then
              sub( im, x, y )-sub( im, x, y-1 )
            else
              ( sub( im, x, y+1 )-sub( im, x, y-1 ) )/2.0 )
        out
  in
    out
  end

  fun packBooleanIntoWord8( images : BooleanImage.image list )
    : Word8GrayscaleImage.image * Word.word =
  let 

    val num =
      if List.length images>8 then
        ( print"More truths than possible"; 0w8 )
      else
        Word.fromInt( List.length images )

    val ( { width, height, ... } )::_ = images

    val out = Word8GrayscaleImage.image( width, height, 0w0)

    fun pack( images, index ) : unit =
      case images of 
        [] => ()
      | im::images' => 
          case index<0w8 of
            false => ()
          | true =>
            let
              val _ =
                Word8GrayscaleImage.modifyxy
                  ( fn( x, y, w ) =>
                    let
                      val Z = 
                        if BooleanImage.sub( im, x, y ) then 
                          0w1 
                        else 
                          0w0
                    in
                      Word8.orb( Word8.<<( Z, index ), w )
                    end )
                  out
            in
              pack( images', index+0w1 ) 
            end

    val _ = pack( images, 0w0 )
  in
    ( out, num )
  end

  fun unpackBooleanFromWord8( im : Word8GrayscaleImage.image, 
                              num : Word.word )
    : BooleanImage.image list =
  let 
    val { width, height, ... } = im

    fun unpack( index ) : BooleanImage.image list =
      case index<num of 
        false => []
      | true => 
        let
          val out = BooleanImage.image( width, height, false )
          val _ =
            BooleanImage.modifyxy
              ( fn( x, y, _ ) =>
                let
                  val w = 
                    Word8.>>( 
                      Word8GrayscaleImage.sub( im, x, y ), 
                      index ) mod 0w2
                in
                  if w>0w0 then
                    true
                  else
                    false
                end )
              out
        in
          out::unpack( index+0w1 ) 
        end
  in
    unpack 0w0
  end

end (* structure ImageUtil *)
