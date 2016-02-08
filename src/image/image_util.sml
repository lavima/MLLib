(*
* file: image_util.sml
* author: Lars Vidar Magnusson
*
* This file contains a structure with utilitary functions for working with
* images 
*)

structure ImageUtil =
struct
  
  fun convertGrayscaleRealToBoolean( Image : GrayscaleImageReal.image )
      : BooleanImage.image = 
  let
    val { Width, Height, ... } = Image

    val Out = BooleanImage.image( Width, Height, false )
    val _ =
      GrayscaleImageReal.appxy
        ( fn( X, Y, W ) =>
            if W>0.0 then
              BooleanImage.update( Out, X, Y, true )
            else
              () )
  in
    Out
  end

  fun convertGrayscaleWord8ToReal( Image : GrayscaleImageWord8.image )
      : GrayscaleImageReal.image =
  let
    val { Width, Height, ... } = Image

    val Out = GrayscaleImageReal.image( Width, Height, 0.0 )
    val _ = 
      GrayscaleImageWord8.appxy
        ( fn( X, Y, W ) =>
            GrayscaleImageReal.update( Out, X, Y, 
              real( Word8.toInt W )/255.0 ) )
        Image
  in
    Out
  end

  fun convertBooleanToReal( Image : BooleanImage.image )
      : GrayscaleImageReal.image =
  let
    val { Width, Height, ... } = Image

    val Out = GrayscaleImageReal.image( Width, Height, 0.0 )
    val _ = 
      BooleanImage.appxy
        ( fn( X, Y, W ) =>
            if W then
              GrayscaleImageReal.update( Out, X, Y, 1.0 )
            else
              GrayscaleImageReal.update( Out, X, Y, 0.0 ) )
        Image
  in
    Out
  end

  fun convertBooleanToTransposedReal( Image : BooleanImage.image )
      : GrayscaleImageReal.image =
  let
    val { Width, Height, ... } = Image

    val Out = GrayscaleImageReal.image( Height, Width, 0.0 )
    val _ = 
      BooleanImage.appxy
        ( fn( X, Y, W ) =>
            if W then
              GrayscaleImageReal.update( Out, Y, X, 1.0 )
            else
              GrayscaleImageReal.update( Out, Y, X, 0.0 ) )
        Image
  in
    Out
  end

  fun copyBooleanToTransposedReal( Src : BooleanImage.image, 
                                   Dst : GrayscaleImageReal.image )
      : unit =
    BooleanImage.appxy
      ( fn( X, Y, W ) =>
          if W then
            GrayscaleImageReal.update( Dst, Y, X, 1.0 )
          else
            GrayscaleImageReal.update( Dst, Y, X, 0.0 ) )
      Src

  (*
  * Normalize an image of reals to the interval [0.0 1.0]. Note that all pixels 
  * are assumed to be equal to or larger than 0.0.
  *)
  fun normalizeReal( Image : GrayscaleImageReal.image )
      : GrayscaleImageReal.image =
  let

    val { Width, Height, ... } = Image

    val Normalized = GrayscaleImageReal.image( Width, Height, 0.0 )

    val Max = GrayscaleImageReal.foldl
      ( fn( X, Max ) => 
          if X>Max then
            X
          else
            Max )
      0.0 
      Image

    val _ = 
      if Max>0.0 then
        GrayscaleImageReal.appi
          ( fn( I, X ) =>
              GrayscaleImageReal.update'( Normalized, I, X/Max ) )
          Image
      else
        ()
  in
    Normalized
  end

  fun normalizeReal'( Image : GrayscaleImageReal.image )
      : unit =
  let

    val { Width, Height, ... } = Image

    val Max = GrayscaleImageReal.foldl
      ( fn( X, Max ) => 
          if X>Max then
            X
          else
            Max )
      0.0 
      Image

    val _ = 
      if Max>0.0 then
        GrayscaleImageReal.modifyi
          ( fn( I, X ) => X/Max )
          Image
      else
        ()
  in
    ()
  end

  (*
   * Normalize an image of reals by translating and scaling.
   *)
  fun normalizeReal''( Image : GrayscaleImageReal.image )
       : GrayscaleImageReal.image =
  let

    val { Width, Height, ... } = Image

    val Normalized = GrayscaleImageReal.image( Width, Height, 0.0 )    

    val Max = GrayscaleImageReal.foldl Real.max Real.negInf Image
    val Min = GrayscaleImageReal.foldl Real.min Real.posInf Image

    val _ = GrayscaleImageReal.appi
          ( fn( I, X ) =>
            GrayscaleImageReal.update'( Normalized, I, (X - Min)/(Max - Min) ) )
          Image
  in
    Normalized
  end

  (*
   * Normalize an image by making it zero mean and l1 norm
   *)
  fun MakeRealZeroMean'( image : GrayscaleImageReal.image )
       : unit =
  let

    val { Width=width, Height=height, ... } = image

    val mean = (GrayscaleImageReal.foldl 
                   (fn (x, a) => x + a) 0.0 image) / (real (width * height))

  in
     GrayscaleImageReal.modify (fn x => x - mean) image    
  end

  fun MakeRealL1Norm'( image : GrayscaleImageReal.image )
       : unit =
  let
    val sum = (GrayscaleImageReal.foldl 
                   (fn (x, a) => Real.abs(x) + a) 0.0 image)
  in
     GrayscaleImageReal.modify (fn x => x / sum) image    
  end

  

  fun gradientXReal( Image : GrayscaleImageReal.image )
      : GrayscaleImageReal.image =
  let

    val sub = GrayscaleImageReal.sub
    val { Width, Height, ... } = Image

    val Out = GrayscaleImageReal.zeroImage( Width, Height )
    val _ = 
      GrayscaleImageReal.modifyxy
        ( fn( X, Y, _ ) => 
            if X=0 then
              sub( Image, X+1, Y )-sub( Image, X, Y )
            else if X=Width-1 then
              sub( Image, X, Y )-sub( Image, X-1, Y )
            else
              ( sub( Image, X+1, Y )-sub( Image, X-1, Y ) )/2.0 )
        Out
  in
    Out
  end

  fun gradientYReal( Image : GrayscaleImageReal.image )
      : GrayscaleImageReal.image =
  let

    val sub = GrayscaleImageReal.sub
    val { Width, Height, ... } = Image

    val Out = GrayscaleImageReal.zeroImage( Width, Height )
    val _ = 
      GrayscaleImageReal.modifyxy
        ( fn( X, Y, _ ) => 
            if Y=0 then
              sub( Image, X, Y+1 )-sub( Image, X, Y )
            else if X=Width-1 then
              sub( Image, X, Y )-sub( Image, X, Y-1 )
            else
              ( sub( Image, X, Y+1 )-sub( Image, X, Y-1 ) )/2.0 )
        Out
  in
    Out
  end

  fun packBooleanIntoWord8( Images : BooleanImage.image list )
    : GrayscaleImageWord8.image * Word.word =
  let 

    val Num =
      if List.length Images>8 then
        ( print"More truths than possible"; 0w8 )
      else
        Word.fromInt( List.length Images )

    val ( Image1 as { Width, Height, ... } )::_ = Images

    val Out = GrayscaleImageWord8.image( Width, Height, 0w0)

    fun pack( Images, Index ) : unit =
      case Images of 
        [] => ()
      | Image::RImages => 
          case Index<0w8 of
            false => ()
          | true =>
            let
              val _ =
                GrayscaleImageWord8.modifyxy
                  ( fn( X, Y, W ) =>
                    let
                      val Z = 
                        if BooleanImage.sub( Image, X, Y ) then 
                          0w1 
                        else 
                          0w0
                    in
                      Word8.orb( Word8.<<( Z, Index ), W )
                    end )
                  Out
            in
              pack( RImages, Index+0w1 ) 
            end

    val _ = pack( Images, 0w0 )
  in
    ( Out, Num )
  end

  fun unpackBooleanFromWord8( Image : GrayscaleImageWord8.image, 
                              Num : Word.word )
    : BooleanImage.image list =
  let 
    val { Width, Height, ... } = Image

    fun unpack( Index ) : BooleanImage.image list =
      case Index<Num of 
        false => []
      | true => 
        let
          val Out = BooleanImage.image( Width, Height, false )
          val _ =
            BooleanImage.modifyxy
              ( fn( X, Y, _ ) =>
                let
                  val W = 
                    Word8.>>( 
                      GrayscaleImageWord8.sub( Image, X, Y ), 
                      Index ) mod 0w2
                in
                  if W>0w0 then
                    true
                  else
                    false
                end )
              Out
        in
          Out::unpack( Index+0w1 ) 
        end
  in
    unpack 0w0
  end

end (* structure ImageUtil *)
