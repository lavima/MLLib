(* 
* filename: pnm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains an SML implementation of the Portable aNy Map image 
* formats. 
*)

structure PNM =
struct

  exception pnmException of string

  datatype format = 
    plainPBM | 
    plainPGM | 
    plainPPM | 
    rawPBM | 
    rawPGM | 
    rawPPM | 
    rawPAM of int

  fun getDepth( f : format ) : int =
    case f of
      plainPBM => 1
    | plainPGM => 1
    | plainPPM => 3
    | rawPBM => 1
    | rawPGM => 1
    | rawPPM => 3
    | rawPAM depth => depth


  fun load( filename : string ) : 'a Image.image = 
  let
    val input = BinIO.openIn( filename )

    val ( format, width, height, maxVal, tupleTypes ) = 
      PNMText.parseHeader input

    val depth = PNMCommon.getDepth format

    val im = 
      if format=plainPGM orelse format=plainPPM then
        Image.createImage( width, height, 
          List.map 
            ( fn ws => Image.pixelFromWords( ws, maxVal, false ) ) 
            ( PNMText.parsePixels( input, depth, false ) ) )
        handle 
          ImageCommon.formatException msg => raise pnmException msg
        | ImageCommon.createException msg => raise pnmException msg
        | pnmException msg => raise pnmException msg
        | _ => raise pnmException"Unknown"
      else if format=plainPBM then
        Image.createImage( width, height, 
          List.map 
            ( fn ws => Image.pixelFromWords( ws, maxVal, true ) )
            ( PNMText.parsePixels( input, depth, true ) ) )
        handle 
          pnmException msg => raise pnmException msg
        | _ => raise pnmException"Unknown"
      else if format=rawPBM then
        Image.createImage( width, height, 
          List.map 
            ( fn ws => Image.pixelFromWords( ws, maxVal, true ) )
            ( PNMBinary.readPixelsAsBits( input, width, height ) ) )
        handle 
          pnmException msg => raise pnmException msg
        | _ => raise pnmException"Unknown"
      else
        Image.createImage( width, height, 
          List.map 
            ( fn ws => Image.pixelFromWords( ws, maxVal, false ) )
            ( PNMBinary.readPixelsAsBytes( input, depth, maxVal, width*height ) ) )
        handle 
          pnmException msg => raise pnmException msg
        | _ => raise pnmException"Unknown"

    val _ = BinIO.closeIn input 
  in
    im
  end
       

  fun save( im as { width, height, values } : 'a Image.image, 
            filename : string, format : PNMCommon.format, maxVal : word ) 
      : unit = 
  let
    val output = BinIO.openOut filename 
    val pixelWords = 
      Array.foldr 
        ( fn( p, ps ) => 
            if format=PNMCommon.plainPBM orelse format=PNMCommon.rawPBM then
              Image.pixelToWords( p, maxVal, true )::ps
            else
              Image.pixelToWords( p, maxVal, false )::ps )
        []
        values
  in (
    PNMText.writeHeader( output, 
      ( format, width, height, Image.depth, maxVal, [] ) );
    if ( format=plainPBM orelse format=plainPGM orelse format=plainPPM ) then
      PNMText.writePixels( output, pixelWords )
    else if format=rawPBM then
      PNMBinary.writePixelsAsBits( output, width, pixelWords )
    else
      PNMBinary.writePixelsAsBytes( output, maxVal, pixelWords ) ;
    BinIO.closeOut output )
  end

end (* structure PNM *)
