(* 
* filename: pnm_text.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure that provides a simple parser for parsing the 
* ASCII formatted headers of the PNM files, and, if the files are formatted in 
* plain format, the ASCII formatted pixel data.
*
* Due to the way PNM files are formatted, the PNM ASCII parser utilize a 
* simple wrapper over BinIO instead of using TextIO like expected.
*)


structure PNMText =
struct

  open PNMCommon

  local
    
    datatype state = 
      getFormat | 
      getIdentifier | 
      getWidth | 
      getHeight | 
      getDepth |
      getMaxVal |
      getTupleType 


    fun input1( inp : BinIO.instream ) : char option =
      case BinIO.input1 inp of 
        NONE => NONE
      | SOME w => SOME( Char.chr( Word8.toInt w ) )

    fun lookahead( inp : BinIO.instream ) : char option =
      case BinIO.lookahead inp of 
        NONE => NONE
      | SOME w => SOME( Char.chr( Word8.toInt w ) )

    fun output( out : BinIO.outstream, s : string ) : unit =
      List.app
        ( fn c => BinIO.output1( out, Word8.fromInt( Char.ord c ) ) )
        ( String.explode s )


    fun wordFromString( s : string ) : word = 
      case StringCvt.scanString ( Word.scan StringCvt.DEC ) s of
        NONE => raise pnmException("Couldn't convert " ^ s ^ " to a word." )
      | SOME w => w

    fun getToken( inp : BinIO.instream, parseBits : bool ) : string option = 
    let
      fun readWhites( inComment : bool ) : unit =
        case lookahead( inp ) of 
          NONE => ()
        | SOME c =>
            if Char.isSpace c orelse inComment then 
              ( input1 inp; 
                readWhites( if c= #"\n" then false else inComment ) )
            else if c= #"#" then
              ( input1 inp; readWhites true )
            else
              ()

      fun buildToken() : string list option =
        case input1 inp of 
          NONE => NONE
        | SOME c =>
            if parseBits then
              SOME [ String.str c ]
            else if Char.isSpace c then
              SOME []
            else if c= #"#" then
              ( readWhites true; buildToken() )
            else
              case buildToken() of 
                NONE => SOME [ String.str c ]
              | SOME cs => SOME( String.str c::cs )

      val _ = readWhites false
    in
      case buildToken() of 
        NONE => NONE
      | SOME cs => SOME( String.concat cs )
    end

    fun parseFormat( inp : BinIO.instream ) : format =
      case getToken( inp, false ) of
        NONE => raise pnmException"Could not read format token."
      | SOME token =>
          case token of 
            "P1" => plainPBM
          | "P2" => plainPGM
          | "P3" => plainPPM
          | "P4" => rawPBM
          | "P5" => rawPGM
          | "P6" => rawPPM
          | "P7" => rawPAM 0
          | _ => raise pnmException( "Wrong magic number: " ^ token )

    fun writeFormat( out : BinIO.outstream, fmt : format ) : unit =
      case fmt of
        plainPBM => output( out, "P1" )
      | plainPGM => output( out, "P2" )
      | plainPPM => output( out, "P3" )
      | rawPBM => output( out, "P4" )
      | rawPGM => output( out, "P5" )
      | rawPPM => output( out, "P6" )
      | rawPAM _ => output( out, "P7" )

    fun parseInt( inp : BinIO.instream ) : int =
      case getToken( inp, false ) of 
        NONE => raise pnmException"Could not read integer token."
      | SOME token =>
          case Int.fromString( token ) of
            SOME x => x
          | NONE => raise pnmException( token ^ " is not an integer" )

    fun writeInt( out : BinIO.outstream, x : int ) : unit =
      output( out, Int.toString x )

    fun parseWord( inp : BinIO.instream ) : word =
      case getToken( inp, false ) of 
        NONE => raise pnmException"Could not read unsigned integer token."
      | SOME token => wordFromString token
      
    fun writeWord( out : BinIO.outstream, x : word ) : unit =
      output( out, Word.fmt StringCvt.DEC x )

  in

    fun parseHeader( inp : BinIO.instream ) 
        : format * int * int * word * string list =
    let
      fun parse( inp : BinIO.instream,
                 s : state, 
                 im as 
                  ( fmt : format, 
                    width : int, 
                    height : int, 
                    maxVal : word,
                    tupleTypes : string list ) )
          : format * int * int * word * string list =
      case s of 
        getFormat => ( 
        let
          val fmt' = parseFormat inp
        in
          case fmt of
            rawPAM _ =>
              parse( inp, getIdentifier, 
                ( fmt', width, height, maxVal, tupleTypes ) )
          | _ =>
              parse( inp, getWidth, 
                ( fmt', width, height, maxVal, tupleTypes ) )
        end )
      | getIdentifier => (
          case getToken( inp, false ) of 
            NONE => raise pnmException"Could not read identifier token"
          | SOME token =>
              case token of 
                "WIDTH" => parse( inp, getWidth, im )
              | "HEIGHT" => parse( inp, getHeight, im )
              | "DEPTH" => parse( inp, getDepth, im )
              | "MAXVAL" => parse( inp, getMaxVal, im )
              | "TUPLTYPE" => parse( inp, getTupleType, im )
              | "ENDHDR" => 
                  ( fmt, width, height, maxVal, 
                      List.rev tupleTypes ) )
      | getWidth => ( 
          parse( inp, 
            case fmt of rawPAM _ => getIdentifier | _ => getHeight, 
            ( fmt, parseInt inp, height, maxVal, tupleTypes ) ) )
      | getHeight => (
          if fmt=rawPBM orelse fmt=plainPBM then
            ( fmt, width, parseInt inp, maxVal, tupleTypes )
          else
            parse( inp, 
              case fmt of rawPAM _ => getIdentifier | _ => getMaxVal, 
              ( fmt, width, parseInt inp, maxVal, tupleTypes ) ) )
      | getDepth => 
        let
          val depth = parseInt inp
        in 
          parse( inp, getIdentifier,
            ( rawPAM depth, width, height, maxVal, tupleTypes ) )
        end
      | getMaxVal => (
          case fmt of
            rawPAM _ =>
              parse( inp, getIdentifier,  
                ( fmt, width, height, parseWord inp, tupleTypes ) )
          | _ =>
            ( fmt, width, height, parseWord inp, List.rev tupleTypes ) )
      | getTupleType => (
          case getToken( inp, false ) of 
            NONE => raise pnmException"Could not read tuple type token."
          | SOME token =>
              parse( inp, getIdentifier, 
                ( fmt, width, height, maxVal, token::tupleTypes ) ) )
    in
      parse( inp, getFormat, ( plainPBM, 0, 0, 0w0, [] ) ) 
    end

    fun writeHeader( out : BinIO.outstream, 
                     im as ( fmt : format, width : int, height : int, 
                       depth : int, maxVal : word, tupleTypes : string list ) )
        : unit = 
    let
      val _ = 
        if not( ( PNMCommon.getDepth fmt )=depth ) then
          raise pnmException
            "The depth of the image does not match the depth of the format"
        else
          ()
    in
      case fmt of
        rawPAM depth => (
          writeFormat( out, fmt );
          output( out, "\nWIDTH ");
          writeInt( out, width );
          output( out, "\nHEIGHT ");
          writeInt( out, height );
          output( out, "\nDEPTH ");
          writeInt( out, depth );
          output( out, "\nMAXVAL ");
          writeWord( out, maxVal );
          output( out, "\nENDHDR\n") )
      | _ => (
          writeFormat( out, fmt );
          output( out, " ");
          writeInt( out, width );
          output( out, " ");
          writeInt( out, height );
          if not( fmt=rawPBM orelse fmt=plainPBM ) then (
            output( out, " ");
            writeWord( out, maxVal ) )
          else
            () ; 
          output( out, "\n" ) )
    end


    fun parsePixels( inp : BinIO.instream, depth : int, parseBits : bool )
        : word list list = 
    let
      fun parse( tokens : string list )
          : word list list =
        case tokens of 
          [] => (
            case getToken( inp, parseBits ) of 
              NONE => []
            | SOME token => parse [ token ] )
        | _ => (
            case List.length tokens<depth of
              false => 
                ( List.map ( fn x => wordFromString x ) ( List.rev tokens ) ) 
                  :: parse []
            | true => ( 
                case getToken( inp, parseBits ) of 
                  NONE => 
                    raise pnmException"Not enough input tokens for pixel." 
                | SOME token =>
                    parse( token::tokens ) ) )
    in
      parse []
    end

    fun writePixels( out : BinIO.outstream, pixels : word list list ) 
        : unit =
      case pixels of 
        [] => ()
      | ws::pixels' => 
        let
          fun writeWords( ws : word list ) : unit =
            case ws of 
              [] => ()
            | w::ws' => (
                writeWord( out, w );
                output( out, " " );
                writeWords ws' )
        in (
          writeWords ws;
          output( out, "\n" );
          writePixels( out, pixels' ) )
        end

  end (* local *)

end (* structure PNMParser *)
