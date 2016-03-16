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
    val ( height, width ) = RealGrayscaleImage.dimensions im

    val out = BooleanImage.image( width, height, false )
    val _ =
      RealGrayscaleImage.appi RealGrayscaleImage.RowMajor
        ( fn( y, x, w ) =>
            if w>0.0 then
              BooleanImage.update( out, y, x, true )
            else
              () )
        ( RealGrayscaleImage.full im )

  in
    out
  end

  fun convertGrayscaleWord8ToReal( im : Word8GrayscaleImage.image )
      : RealGrayscaleImage.image =
  let
    val ( height, width ) = Word8GrayscaleImage.dimensions im

    val out = RealGrayscaleImage.image( width, height, 0.0 )
    val _ = 
      Word8GrayscaleImage.appi Word8GrayscaleImage.RowMajor
        ( fn( y, x, w ) =>
            RealGrayscaleImage.update( out, y, x, 
              real( Word8.toInt w )/255.0 ) )
        ( Word8GrayscaleImage.full im )
  in
    out
  end

  fun convertBooleanToReal( im : BooleanImage.image )
      : RealGrayscaleImage.image =
  let
    val ( height, width ) = BooleanImage.dimensions im

    val out = RealGrayscaleImage.image( width, height, 0.0 )
    val _ = 
      BooleanImage.appi BooleanImage.RowMajor
        ( fn( y, x, w ) =>
            if w then
              RealGrayscaleImage.update( out, y, x, 1.0 )
            else
              RealGrayscaleImage.update( out, y, x, 0.0 ) )
        ( BooleanImage.full im )
  in
    out
  end

  fun convertBooleanToTransposedReal( im : BooleanImage.image )
      : RealGrayscaleImage.image =
  let
    val ( height, width ) = BooleanImage.dimensions im

    val out = RealGrayscaleImage.image( height, width, 0.0 )
    val _ = 
      BooleanImage.appi BooleanImage.RowMajor
        ( fn( y, x, w ) =>
            if w then
              RealGrayscaleImage.update( out, x, y, 1.0 )
            else
              RealGrayscaleImage.update( out, x, y, 0.0 ) )
        ( BooleanImage.full im )
  in
    out
  end

  fun copyBooleanToTransposedReal( src : BooleanImage.image, 
                                   dst : RealGrayscaleImage.image )
      : unit =
    BooleanImage.appi BooleanImage.RowMajor
      ( fn( y, x, w ) =>
          if w then
            RealGrayscaleImage.update( dst, x, y, 1.0 )
          else
            RealGrayscaleImage.update( dst, x, y, 0.0 ) )
      ( BooleanImage.full src )

  (*
  * Normalize an image of reals to the interval [0.0 1.0]. Note that all pixels 
  * are assumed to be equal to or larger than 0.0.
  *)
  fun normalizeReal( im : RealGrayscaleImage.image )
      : RealGrayscaleImage.image =
  let

    val ( height, width ) = RealGrayscaleImage.dimensions im

    val normalized = RealGrayscaleImage.image( width, height, 0.0 )

    val max = RealGrayscaleImage.fold RealGrayscaleImage.RowMajor
      ( fn( x, max ) => 
          if x>max then
            x
          else
            max )
      0.0 
      im

    val _ = 
      if max>0.0 then
        RealGrayscaleImage.appi RealGrayscaleImage.RowMajor
          ( fn( i, j, x ) =>
              RealGrayscaleImage.update( normalized, i, j, x/max ) )
          ( RealGrayscaleImage.full im )
      else
        ()
  in
    normalized
  end

  fun normalizeReal'( im : RealGrayscaleImage.image )
      : unit =
  let

    val ( height, width ) = RealGrayscaleImage.dimensions im

    val max = RealGrayscaleImage.fold RealGrayscaleImage.RowMajor
      ( fn( x, max ) => 
          if x>max then
            x
          else
            max )
      0.0 
      im

    val _ = 
      if max>0.0 then
        RealGrayscaleImage.modify RealGrayscaleImage.RowMajor
          ( fn x => x/max )
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

    val ( height, width ) = RealGrayscaleImage.dimensions im

    val normalized = RealGrayscaleImage.image( width, height, 0.0 )    

    val max = 
      RealGrayscaleImage.fold RealGrayscaleImage.RowMajor 
        Real.max 
        Real.negInf 
        im
    val min = 
      RealGrayscaleImage.fold RealGrayscaleImage.RowMajor 
        Real.min 
        Real.posInf 
        im

    val _ = 
      RealGrayscaleImage.appi RealGrayscaleImage.RowMajor
        ( fn( i, j, x ) =>
          RealGrayscaleImage.update( 
            normalized, 
            i, j, 
            ( x-min)/( max-min ) ) )
        ( RealGrayscaleImage.full im )
  in
    normalized
  end

  (*
   * Normalize an image by making it zero mean and l1 norm
   *)
  fun makeRealZeroMean'( image : RealGrayscaleImage.image )
       : unit =
  let

    val ( height, width ) = RealGrayscaleImage.dimensions image

    val mean = 
      ( RealGrayscaleImage.fold RealGrayscaleImage.RowMajor 
          ( fn ( x, a ) => x+a ) 
          0.0 
          image ) 
      / 
      ( real( width*height ) )

  in
     RealGrayscaleImage.modify RealGrayscaleImage.RowMajor 
      ( fn x => x-mean ) 
      image    
  end

  fun makeRealL1Norm'( image : RealGrayscaleImage.image )
       : unit =
  let
    val sum = 
      RealGrayscaleImage.fold RealGrayscaleImage.RowMajor 
        ( fn( x, a ) => ( Real.abs x )+a ) 
        0.0 
        image 
  in
     RealGrayscaleImage.modify RealGrayscaleImage.RowMajor 
      ( fn x => x/sum ) 
      image    
  end

  fun gradientXReal( im : RealGrayscaleImage.image )
      : RealGrayscaleImage.image =
  let

    val sub = RealGrayscaleImage.sub
    val ( height, width ) = RealGrayscaleImage.dimensions im

    val out = RealGrayscaleImage.zeroImage( width, height )
    val _ = 
      RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor
        ( fn( y, x, _ ) => 
            if x=0 then
              sub( im, y, x+1 )-sub( im, y, x )
            else if x=width-1 then
              sub( im, y, x )-sub( im, y, x-1 )
            else
              ( sub( im, y, x+1 )-sub( im, y, x-1 ) )/2.0 )
        ( RealGrayscaleImage.full out )
  in
    out
  end

  fun gradientYReal( im : RealGrayscaleImage.image )
      : RealGrayscaleImage.image =
  let

    val sub = RealGrayscaleImage.sub
    val ( height, width ) = RealGrayscaleImage.dimensions im

    val out = RealGrayscaleImage.zeroImage( width, height )
    val _ = 
      RealGrayscaleImage.modifyi RealGrayscaleImage.RowMajor
        ( fn( y, x, _ ) => 
            if y=0 then
              sub( im, y+1, x )-sub( im, y, x )
            else if x=width-1 then
              sub( im, y, x )-sub( im, y-1, x )
            else
              ( sub( im, y+1, x )-sub( im, y-1, x ) )/2.0 )
        ( RealGrayscaleImage.full out )
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

    val im::_ = images 
    val ( height, width ) = BooleanImage.dimensions im

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
                Word8GrayscaleImage.modifyi Word8GrayscaleImage.RowMajor
                  ( fn( y, x, w ) =>
                    let
                      val z = 
                        if BooleanImage.sub( im, y, x ) then 
                          0w1 
                        else 
                          0w0
                    in
                      Word8.orb( Word8.<<( z, index ), w )
                    end )
                  ( Word8GrayscaleImage.full out )
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
    val ( height, width ) = Word8GrayscaleImage.dimensions im

    fun unpack( index ) : BooleanImage.image list =
      case index<num of 
        false => []
      | true => 
        let
          val out = BooleanImage.image( width, height, false )
          val _ =
            BooleanImage.modifyi BooleanImage.RowMajor
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
              ( BooleanImage.full out )
        in
          out::unpack( index+0w1 ) 
        end
  in
    unpack 0w0
  end

end (* structure ImageUtil *)
