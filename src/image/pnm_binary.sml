(* 
* filename: pnm_binary.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure that provides a binary reader for PNM raster
* data.
*)

structure PNMBinary =
struct

  open PNMCommon


  local 

    fun readWord( Input : BinIO.instream, MaxVal : word ) : word =
    let
      val wfw8 = Word.fromInt o Word8.toInt
    in
      case BinIO.input1 Input of
        NONE => raise pnmException"Couldn't read first byte of word"
      | SOME W1 =>
          if MaxVal<0w256 then
            wfw8 W1
          else 
            case BinIO.input1 Input of
              NONE => raise pnmException"Couldn't read second byte of word"
            | SOME W2 =>
                Word.orb( Word.<<( wfw8 W1, 0w8 ), wfw8 W2 )
    end

    fun writeWord( Output : BinIO.outstream, MaxVal : word, W : word ) : unit =
    let
      val w8fw = Word8.fromInt o Word.toInt
    in
      if MaxVal<0w256 then
        BinIO.output1( Output, w8fw W )
      else (
        BinIO.output1( Output, w8fw( Word.>>( W, 0w8 ) ) );
        BinIO.output1( Output, w8fw( W mod 0w256 ) ) )
    end

    fun readPixel( Input : BinIO.instream, Depth : int, MaxVal : word ) 
        : word list = 
    let 
      fun buildPixel( Index : int ) : word list = 
        case Index<Depth of 
          false => []
        | true => readWord( Input, MaxVal )::buildPixel( Index+1 ) 
    in
      buildPixel 0
    end

    fun writePixel( Output : BinIO.outstream, MaxVal : word, Words : word list )
        : unit =
      case Words of 
        [] => ()
      | W::RWords => ( 
          writeWord( Output, MaxVal, W ); 
          writePixel( Output, MaxVal, RWords ) )

  in

    fun readPixelsAsBytes( Input : BinIO.instream, 
                           Depth : int, MaxVal : word, NumPixels : int ) 
        : word list list =
    let
      fun read( Index : int ) : word list list =
        case Index<NumPixels of 
          false => []
        | true => readPixel( Input, Depth, MaxVal )::read( Index+1 )
    in
      read 0
    end


    fun writePixelsAsBytes( Output : BinIO.outstream, 
                            MaxVal : word, 
                            Pixels : word list list ) 
        : unit =
      case Pixels of
        [] => ()
      | Pixel::RPixels => writePixel( Output, MaxVal, Pixel )

  end (* local *)

  local 

    val CurrentIn : Word8.word ref = ref 0w0

    fun readPixel( Input : BinIO.instream, X : int ) 
        : word list = 
    let 
      val wfw8 = Word.fromInt o Word8.toInt

      val I = Word.fromInt( X mod 8 )
      val _ = 
        if I=0w0 then
          case BinIO.input1 Input of
            NONE => raise pnmException"Unable to read new byte from bitstream"
          | SOME W => 
              CurrentIn := W
        else
          ()
    in
      [ wfw8( Word8.>>( !CurrentIn, 0w8-I-0w1 ) mod 0w2 ) ]
    end

    val CurrentOut : Word8.word ref = ref 0w0

    fun writePixel( Output : BinIO.outstream, X : int, Ws : word list ) 
        : unit = 
    let 
      val w8fw = Word8.fromInt o Word.toInt

      val I = Word.fromInt( X mod 8 )

      val _ = 
        if I=0w0 andalso X>0 then (
          BinIO.output1( Output, !CurrentOut );
          CurrentOut := 0w0 )
        else if I=0w0 then
          CurrentOut := 0w0
        else
          ()

      val [ W1 ] = Ws
      val W : Word8.word = 
        if W1>0w0 then
          0w1
        else
          0w0

    in
      CurrentOut := Word8.orb( !CurrentOut, Word8.<<( W, 0w8-I-0w1 ) )
    end

    fun flush( Output : BinIO.outstream ) : unit =
      BinIO.output1( Output, !CurrentOut )

  in

    fun readPixelsAsBits( Input : BinIO.instream,
                          Width : int, Height : int ) 
        : word list list =
    let
      val NumPixels = Width*Height

      fun read( Index : int ) : word list list =
      let
        val X = Index mod Width
      in
        case Index<NumPixels of 
          false => []
        | true => readPixel( Input, X )::read( Index+1 )
      end
    in
      read 0
    end

    fun writePixelsAsBits( Output : BinIO.outstream, 
                           Width : int, Pixels : word list list )
        : unit =
    let
      fun write( Index : int, Wss : word list list ) : unit =
      let
        val X = Index mod Width
        val _ = 
          if X=0 andalso Index>0 then
            flush Output
          else
            ()
      in
        case Wss of
          [] => ( )
        | Ws::RWss => (
            writePixel( Output, X, Ws );
            write( Index+1, RWss ) )
      end
    in
      write( 0, Pixels )
    end

  end (* local *)

end (* struct PNMBinary *)
