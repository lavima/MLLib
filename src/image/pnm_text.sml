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


    fun input1( Input : BinIO.instream ) : char option =
      case BinIO.input1 Input of 
        NONE => NONE
      | SOME W => SOME( Char.chr( Word8.toInt W ) )

    fun lookahead( Input : BinIO.instream ) : char option =
      case BinIO.lookahead Input of 
        NONE => NONE
      | SOME W => SOME( Char.chr( Word8.toInt W ) )

    fun output( Output : BinIO.outstream, S : string ) : unit =
      List.app
        ( fn C => BinIO.output1( Output, Word8.fromInt( Char.ord C ) ) )
        ( String.explode S )


    fun wordFromString( S : string ) : word = 
      case StringCvt.scanString ( Word.scan StringCvt.DEC ) S of
        NONE => raise pnmException("Couldn't convert " ^ S ^ " to a word." )
      | SOME W => W

    fun getToken( Input : BinIO.instream, ParseBits : bool ) : string option = 
    let
      fun readWhites( InComment : bool ) : unit =
        case lookahead( Input ) of 
          NONE => ()
        | SOME C =>
            if Char.isSpace C orelse InComment then 
              ( input1 Input; 
                readWhites( if C= #"\n" then false else InComment ) )
            else if C= #"#" then
              ( input1 Input; readWhites true )
            else
              ()

      fun buildToken() : string list option =
        case input1 Input of 
          NONE => NONE
        | SOME C =>
            if ParseBits then
              SOME [ String.str C ]
            else if Char.isSpace C then
              SOME []
            else if C= #"#" then
              ( readWhites true; buildToken() )
            else
              case buildToken() of 
                NONE => SOME [ String.str C ]
              | SOME Cs => SOME( String.str C::Cs )

      val _ = readWhites false
    in
      case buildToken() of 
        NONE => NONE
      | SOME Cs => SOME( String.concat Cs )
    end

    fun parseFormat( Input : BinIO.instream ) : format =
      case getToken( Input, false ) of
        NONE => raise pnmException"Could not read format token."
      | SOME Token =>
          case Token of 
            "P1" => plainPBM
          | "P2" => plainPGM
          | "P3" => plainPPM
          | "P4" => rawPBM
          | "P5" => rawPGM
          | "P6" => rawPPM
          | "P7" => rawPAM 0
          | _ => raise pnmException( "Wrong magic number: " ^ Token )

    fun writeFormat( Output : BinIO.outstream, Format : format ) : unit =
      case Format of
        plainPBM => output( Output, "P1" )
      | plainPGM => output( Output, "P2" )
      | plainPPM => output( Output, "P3" )
      | rawPBM => output( Output, "P4" )
      | rawPGM => output( Output, "P5" )
      | rawPPM => output( Output, "P6" )
      | rawPAM _ => output( Output, "P7" )

    fun parseInt( Input : BinIO.instream ) : int =
      case getToken( Input, false ) of 
        NONE => raise pnmException"Could not read integer token."
      | SOME Token =>
          case Int.fromString( Token ) of
            SOME X => X
          | NONE => raise pnmException( Token ^ " is not an integer" )

    fun writeInt( Output : BinIO.outstream, X : int ) : unit =
      output( Output, Int.toString X )

    fun parseWord( Input : BinIO.instream ) : word =
      case getToken( Input, false ) of 
        NONE => raise pnmException"Could not read unsigned integer token."
      | SOME Token => wordFromString Token
      
    fun writeWord( Output : BinIO.outstream, X : word ) : unit =
      output( Output, Word.fmt StringCvt.DEC X )

  in

    fun parseHeader( Input : BinIO.instream ) 
        : format * int * int * word * string list =
    let
      fun parse( Input : BinIO.instream,
                 State : state, 
                 Image as 
                  ( Format : format, 
                    Width : int, 
                    Height : int, 
                    MaxVal : word,
                    TupleTypes : string list ) )
          : format * int * int * word * string list =
      case State of 
        getFormat => ( 
        let
          val Format' = parseFormat Input
        in
          case Format of
            rawPAM _ =>
              parse( Input, getIdentifier, 
                ( Format', Width, Height, MaxVal, TupleTypes ) )
          | _ =>
              parse( Input, getWidth, 
                ( Format', Width, Height, MaxVal, TupleTypes ) )
        end )
      | getIdentifier => (
          case getToken( Input, false ) of 
            NONE => raise pnmException"Could not read identifier token"
          | SOME Token =>
              case Token of 
                "WIDTH" => parse( Input, getWidth, Image )
              | "HEIGHT" => parse( Input, getHeight, Image )
              | "DEPTH" => parse( Input, getDepth, Image )
              | "MAXVAL" => parse( Input, getMaxVal, Image )
              | "TUPLTYPE" => parse( Input, getTupleType, Image )
              | "ENDHDR" => 
                  ( Format, Width, Height, MaxVal, 
                      List.rev TupleTypes ) )
      | getWidth => ( 
          parse( Input, 
            case Format of rawPAM _ => getIdentifier | _ => getHeight, 
            ( Format, parseInt Input, Height, MaxVal, TupleTypes ) ) )
      | getHeight => (
          if Format=rawPBM orelse Format=plainPBM then
            ( Format, Width, parseInt Input, MaxVal, TupleTypes )
          else
            parse( Input, 
              case Format of rawPAM _ => getIdentifier | _ => getMaxVal, 
              ( Format, Width, parseInt Input, MaxVal, TupleTypes ) ) )
      | getDepth => 
        let
          val Depth = parseInt Input
        in 
          parse( Input, getIdentifier,
            ( rawPAM Depth, Width, Height, MaxVal, TupleTypes ) )
        end
      | getMaxVal => (
          case Format of
            rawPAM _ =>
              parse( Input, getIdentifier,  
                ( Format, Width, Height, parseWord Input, TupleTypes ) )
          | _ =>
            ( Format, Width, Height, parseWord Input, List.rev TupleTypes ) )
      | getTupleType => (
          case getToken( Input, false ) of 
            NONE => raise pnmException"Could not read tuple type token."
          | SOME Token =>
              parse( Input, getIdentifier, 
                ( Format, Width, Height, MaxVal, Token::TupleTypes ) ) )
    in
      parse( Input, getFormat, ( plainPBM, 0, 0, 0w0, [] ) ) 
    end

    fun writeHeader( Output : BinIO.outstream, 
                     Image as ( Format : format, Width : int, Height : int, 
                       Depth : int, MaxVal : word, TupleTypes : string list ) )
        : unit = 
    let
      val _ = 
        if not( ( PNMCommon.getDepth Format )=Depth ) then
          raise pnmException
            "The depth of the image does not match the depth of the format"
        else
          ()
    in
      case Format of
        rawPAM Depth => (
          writeFormat( Output, Format );
          output( Output, "\nWIDTH ");
          writeInt( Output, Width );
          output( Output, "\nHEIGHT ");
          writeInt( Output, Height );
          output( Output, "\nDEPTH ");
          writeInt( Output, Depth );
          output( Output, "\nMAXVAL ");
          writeWord( Output, MaxVal );
          output( Output, "\nENDHDR\n") )
      | _ => (
          writeFormat( Output, Format );
          output( Output, " ");
          writeInt( Output, Width );
          output( Output, " ");
          writeInt( Output, Height );
          if not( Format=rawPBM orelse Format=plainPBM ) then (
            output( Output, " ");
            writeWord( Output, MaxVal ) )
          else
            () ; 
          output( Output, "\n" ) )
    end


    fun parsePixels( Input : BinIO.instream, Depth : int, ParseBits : bool )
        : word list list = 
    let
      fun parse( Tokens : string list )
          : word list list =
        case Tokens of 
          [] => (
            case getToken( Input, ParseBits ) of 
              NONE => []
            | SOME Token => parse [ Token ] )
        | _ => (
            case List.length Tokens<Depth of
              false => 
                ( List.map ( fn X => wordFromString X ) ( List.rev Tokens ) ) 
                  :: parse []
            | true => ( 
                case getToken( Input, ParseBits ) of 
                  NONE => 
                    raise pnmException"Not enough input tokens for pixel." 
                | SOME Token =>
                    parse( Token::Tokens ) ) )
    in
      parse []
    end

    fun writePixels( Output : BinIO.outstream, Pixels : word list list ) 
        : unit =
      case Pixels of 
        [] => ()
      | Ws::RPixels => 
        let
          fun writeWords( Ws : word list ) : unit =
            case Ws of 
              [] => ()
            | W::RWs => (
                writeWord( Output, W );
                output( Output, " " );
                writeWords RWs )
        in (
          writeWords Ws;
          output( Output, "\n" );
          writePixels( Output, RPixels ) )
        end

  end (* local *)

end (* structure PNMParser *)
