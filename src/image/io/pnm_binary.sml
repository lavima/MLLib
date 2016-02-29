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

    fun readWord( input : BinIO.instream, maxVal : word ) : word =
    let
      val wfw8 = Word.fromInt o Word8.toInt
    in
      case BinIO.input1 input of
        NONE => raise pnmException"Couldn't read first byte of word"
      | SOME w1 =>
          if maxVal<0w256 then
            wfw8 w1
          else 
            case BinIO.input1 input of
              NONE => raise pnmException"Couldn't read second byte of word"
            | SOME w2 =>
                Word.orb( Word.<<( wfw8 w1, 0w8 ), wfw8 w2 )
    end

    fun writeWord( output : BinIO.outstream, maxVal : word, w : word ) : unit =
    let
      val w8fw = Word8.fromInt o Word.toInt
    in
      if maxVal<0w256 then
        BinIO.output1( output, w8fw w )
      else (
        BinIO.output1( output, w8fw( Word.>>( w, 0w8 ) ) );
        BinIO.output1( output, w8fw( w mod 0w256 ) ) )
    end

    fun readPixel( input : BinIO.instream, depth : int, maxVal : word ) 
        : word list = 
    let 
      fun buildPixel( index : int ) : word list = 
        case index<depth of 
          false => []
        | true => readWord( input, maxVal )::buildPixel( index+1 ) 
    in
      buildPixel 0
    end

    fun writePixel( output : BinIO.outstream, maxVal : word, words : word list )
        : unit =
      case words of 
        [] => ()
      | w::words' => ( 
          writeWord( output, maxVal, w ); 
          writePixel( output, maxVal, words' ) )

  in

    fun readPixelsAsBytes( input : BinIO.instream, 
                           depth : int, maxVal : word, numPixels : int ) 
        : word list list =
    let
      fun read( index : int ) : word list list =
        case index<numPixels of 
          false => []
        | true => readPixel( input, depth, maxVal )::read( index+1 )
    in
      read 0
    end


    fun writePixelsAsBytes( output : BinIO.outstream, 
                            maxVal : word, 
                            pixels : word list list ) 
        : unit =
    let
      fun write( pixels : word list list ) : unit =
        case pixels of
          [] => ()
        | pixel::pixels' => (
            writePixel( output, maxVal, pixel );
            write pixels' )
    in
      write pixels
    end

  end (* local *)

  local 

    val currentIn : Word8.word ref = ref 0w0

    fun readPixel( input : BinIO.instream, x : int ) : bool = 
    let 
      val wfw8 = Word.fromInt o Word8.toInt

      val i = Word.fromInt( x mod 8 )
      val _ = 
        if i=0w0 then
          case BinIO.input1 input of
            NONE => raise pnmException"Unable to read new byte from bitstream"
          | SOME w => 
              currentIn := w
        else
          ()
    in
      Word8.andb( Word8.>>( !currentIn, 0w8-i-0w1 ), 0w1 )=0w0
    end

    val currentOut : Word8.word ref = ref 0w0

    fun writePixel( output : BinIO.outstream, x : int, ws : word list ) 
        : unit = 
    let 
      val w8fw = Word8.fromInt o Word.toInt

      val i = Word.fromInt( x mod 8 )

      val _ = 
        if i=0w0 andalso x>0 then (
          BinIO.output1( output, !currentOut );
          currentOut := 0w0 )
        else if i=0w0 then
          currentOut := 0w0
        else
          ()

      val [ w1 ] = ws
      val w : Word8.word = 
        if w1>0w0 then
          0w1
        else
          0w0

    in
      currentOut := Word8.orb( !currentOut, Word8.<<( w, 0w8-i-0w1 ) )
    end

    fun flush( output : BinIO.outstream ) : unit =
      BinIO.output1( output, !currentOut )

  in

    fun readPixelsAsBits( input : BinIO.instream, numPixels : int ) 
        : bool list =
    let
      fun read( index : int ) : bool list =
        case index<numPixels of 
          false => []
        | true => readPixel( input, index )::read( index+1 )
    in
      read 0
    end

    fun writePixelsAsBits( output : BinIO.outstream, 
                           width : int, pixels : word list list )
        : unit =
    let
      fun write( index : int, wss : word list list ) : unit =
      let
        val x = index mod width
        val _ = 
          if x=0 andalso index>0 then
            flush output
          else
            ()
      in
        case wss of
          [] => ( )
        | ws::wss' => (
            writePixel( output, x, ws );
            write( index+1, wss' ) )
      end
    in
      write( 0, pixels )
    end

  end (* local *)

end (* struct PNMBinary *)
