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
  
  exception pnmException of string

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

    fun parseFormat( inp : BinIO.instream ) : PNM.format =
      case getToken( inp, false ) of
        NONE => raise pnmException"Could not read format token."
      | SOME token =>
          case token of 
            "P1" => PNM.plainPBM
          | "P2" => PNM.plainPGM
          | "P3" => PNM.plainPPM
          | "P4" => PNM.rawPBM
          | "P5" => PNM.rawPGM
          | "P6" => PNM.rawPPM
          | "P7" => PNM.rawPAM 0
          | _ => raise pnmException( "Wrong magic number: " ^ token )

    fun writeFormat( out : BinIO.outstream, fmt : PNM.format ) : unit =
      case fmt of
        PNM.plainPBM => output( out, "P1" )
      | PNM.plainPGM => output( out, "P2" )
      | PNM.plainPPM => output( out, "P3" )
      | PNM.rawPBM => output( out, "P4" )
      | PNM.rawPGM => output( out, "P5" )
      | PNM.rawPPM => output( out, "P6" )
      | PNM.rawPAM _ => output( out, "P7" )

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

    fun parseNPixels( inp : BinIO.instream, depth : int )
        : word list list = 
    let
      fun parse( tokens : string list )
          : word list list =
        case tokens of 
          [] => (
            case getToken( inp, false ) of 
              NONE => []
            | SOME token => parse [ token ] )
        | _ => (
            case List.length tokens<depth of
              false => 
                ( List.map ( fn x => wordFromString x ) ( List.rev tokens ) ) 
                  :: parse []
            | true => ( 
                case getToken( inp, false ) of 
                  NONE => 
                    raise pnmException"Not enough input tokens for pixel." 
                | SOME token =>
                    parse( token::tokens ) ) )
    in
      parse []
    end

    fun writeNPixels( out : BinIO.outstream, pixels : word list list ) 
        : unit =
    let
      fun write( pixels : word list list ) : unit =
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
            write pixels' )
          end
    in
      write pixels
    end

  in

    fun parseHeader( inp : BinIO.instream ) 
        : PNM.format * int * int * word * string list =
    let
      fun parse( inp : BinIO.instream,
                 s : state, 
                 im as 
                  ( fmt : PNM.format, 
                    width : int, 
                    height : int, 
                    maxVal : word,
                    tupleTypes : string list ) )
          : PNM.format * int * int * word * string list =
      case s of 
        getFormat => ( 
        let
          val fmt' = parseFormat inp
        in
          case fmt of
            PNM.rawPAM _ =>
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
            case fmt of PNM.rawPAM _ => getIdentifier | _ => getHeight, 
            ( fmt, parseInt inp, height, maxVal, tupleTypes ) ) )
      | getHeight => (
          if fmt=PNM.rawPBM orelse fmt=PNM.plainPBM then
            ( fmt, width, parseInt inp, maxVal, tupleTypes )
          else
            parse( inp, 
              case fmt of PNM.rawPAM _ => getIdentifier | _ => getMaxVal, 
              ( fmt, width, parseInt inp, maxVal, tupleTypes ) ) )
      | getDepth => 
        let
          val depth = parseInt inp
        in 
          parse( inp, getIdentifier,
            ( PNM.rawPAM depth, width, height, maxVal, tupleTypes ) )
        end
      | getMaxVal => (
          case fmt of
            PNM.rawPAM _ =>
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
      parse( inp, getFormat, ( PNM.plainPBM, 0, 0, 0w0, [] ) ) 
    end

    fun writeHeader( out : BinIO.outstream, 
                     im as ( fmt : PNM.format, width : int, height : int, 
                       depth : int, maxVal : word, tupleTypes : string list ) )
        : unit = 
    let
      val _ = 
        if not( ( PNM.getDepth fmt )=depth ) then
          raise pnmException
            "The depth of the image does not match the depth of the format"
        else
          ()
    in
      case fmt of
        PNM.rawPAM depth => (
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
          if not( fmt=PNM.rawPBM orelse fmt=PNM.plainPBM ) then (
            output( out, " ");
            writeWord( out, maxVal ) )
          else
            () ; 
          output( out, "\n" ) )
    end

    fun parseBooleanPixels( input : BinIO.instream ) : bool list =
    let
      fun parse() : bool list =
        case getToken( input, true ) of 
          NONE => []
        | SOME token => 
            case token of 
              "1" => false::parse()
            | "0" => true::parse()
    in
      parse()
    end
    
    fun parseGrayscalePixels( input : BinIO.instream ) : word list =
      List.map 
        ( fn( [ X ] ) => X ) 
        ( parseNPixels( input, 1 ) )

    fun parseColorPixels( input : BinIO.instream ) 
        : ( word * word * word ) list =
      List.map 
        ( fn( [ R, G, B ] ) => ( R, G, B ) ) 
        ( parseNPixels( input, 3 ) )


    fun writeBooleanPixels( out : BinIO.outstream, pixels : bool list ) : unit =
    let
      fun write( pixels : bool list ) : unit =
        case pixels of 
          [] => ()
        | pixel::pixels' =>
            case pixel of 
              false => ( output( out, "1\n" ); write pixels' )
            | true => ( output( out, "0\n" ); write pixels' )
    in
      write pixels
    end

    fun writeGrayscalePixels( out : BinIO.outstream, pixels : word list ) 
        : unit =
      writeNPixels( out, List.map ( fn x => [ x ] ) pixels )

    fun writeColorPixels( out : BinIO.outstream, 
                          pixels : ( word * word * word ) list ) 
        : unit =
      writeNPixels( out, List.map ( fn( r, g, b ) => [ r, g, b ] ) pixels )

  end (* local *)

end (* structure PNMParser *)
