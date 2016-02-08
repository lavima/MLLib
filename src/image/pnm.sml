(* 
* filename: pnm.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains an SML implementation of the Portable aNy Map image 
* formats. The implementation of the standard is not fully compliant e.g. the
* lack of support for .pam files.
*)

signature PNM_IMAGE =
sig

  exception pnmImageException of string

  type element
  type pixel
  type image = { Width : int, Height : int, Values : pixel Array.array }

  val createImage : int * int * pixel list -> image

  val pixelFromWords : Word.word list * word * bool -> pixel
  val pixelToWords : pixel * word * bool -> Word.word list

  val Format : PNMCommon.format
  val Depth : int

end

signature PNM =
sig

  exception pnmException of string

  datatype format = 
    plainPBM | 
    plainPGM | 
    plainPPM | 
    rawPBM | 
    rawPGM | 
    rawPPM | 
    rawPAM of int
    
  structure Image : PNM_IMAGE

  val load : string -> Image.image
  val save : Image.image * string * format * word -> unit

end


functor PNMFun( Image : PNM_IMAGE ) : PNM =
struct

  open PNMCommon


  structure Image = Image


  fun load( Filename : string ) : Image.image = 
  let
    val Input = BinIO.openIn( Filename )

    val ( Format, Width, Height, MaxVal, TupleTypes ) = 
      PNMText.parseHeader Input

    val Depth = PNMCommon.getDepth Format

    val Image = 
      if Format=plainPGM orelse Format=plainPPM then
        Image.createImage( Width, Height, 
          List.map 
            ( fn Ws => Image.pixelFromWords( Ws, MaxVal, false ) ) 
            ( PNMText.parsePixels( Input, Depth, false ) ) )
        handle 
          ImageCommon.formatException Msg => raise pnmException Msg
        | ImageCommon.createException Msg => raise pnmException Msg
        | pnmException Msg => raise pnmException Msg
        | _ => raise pnmException"Unknown"
      else if Format=plainPBM then
        Image.createImage( Width, Height, 
          List.map 
            ( fn Ws => Image.pixelFromWords( Ws, MaxVal, true ) )
            ( PNMText.parsePixels( Input, Depth, true ) ) )
        handle 
          pnmException Msg => raise pnmException Msg
        | _ => raise pnmException"Unknown"
      else if Format=rawPBM then
        Image.createImage( Width, Height, 
          List.map 
            ( fn Ws => Image.pixelFromWords( Ws, MaxVal, true ) )
            ( PNMBinary.readPixelsAsBits( Input, Width, Height ) ) )
        handle 
          pnmException Msg => raise pnmException Msg
        | _ => raise pnmException"Unknown"
      else
        Image.createImage( Width, Height, 
          List.map 
            ( fn Ws => Image.pixelFromWords( Ws, MaxVal, false ) )
            ( PNMBinary.readPixelsAsBytes( Input, Depth, MaxVal, Width*Height ) ) )
        handle 
          pnmException Msg => raise pnmException Msg
        | _ => raise pnmException"Unknown"

    val _ = BinIO.closeIn Input 
  in
    Image
  end
       

  fun save( Image as { Width, Height, Values } : Image.image, 
            Filename : string, Format : PNMCommon.format, MaxVal : word ) 
      : unit = 
  let
    val Output = BinIO.openOut Filename 
    val PixelWords = 
      Array.foldr 
        ( fn( P, Ps ) => 
            if Format=PNMCommon.plainPBM orelse Format=PNMCommon.rawPBM then
              Image.pixelToWords( P, MaxVal, true )::Ps
            else
              Image.pixelToWords( P, MaxVal, false )::Ps )
        []
        Values
  in (
    PNMText.writeHeader( Output, 
      ( Format, Width, Height, Image.Depth, MaxVal, [] ) );
    if ( Format=plainPBM orelse Format=plainPGM orelse Format=plainPPM ) then
      PNMText.writePixels( Output, PixelWords )
    else if Format=rawPBM then
      PNMBinary.writePixelsAsBits( Output, Width, PixelWords )
    else
      PNMBinary.writePixelsAsBytes( Output, MaxVal, PixelWords ) ;
    BinIO.closeOut Output )
  end

end (* structure PNM *)
