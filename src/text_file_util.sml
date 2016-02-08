(* 
* file: file_util.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
* 
* This file contains a structure with functionality to read and write data to
* text files.
*)

structure TextFileUtil =
struct
  
  fun readDSV ( isDelim : char -> bool ) 
              ( fromString : string -> 'a ) 
              ( Filename : string )
      : 'a list =
  let

    val In = TextIO.openIn Filename

    fun read() : 'a list = 
      case TextIO.inputLine In of 
        NONE => []
      | SOME Line => 
        let
          val Xs = String.tokens isDelim Line
        in
          ( List.map fromString Xs ) @ read()
        end

    val Xs = read()

    val _ = TextIO.closeIn In

  in
    Xs
  end
  
  fun writeDSV ( Delim : string, Columns : int )
               ( toString : 'a -> string ) 
               ( Xs : 'a list, Filename : string ) : unit =
  let
    val Out = TextIO.openOut Filename

    fun write( Xs : 'a list ) : unit =
      case ( List.length Xs )>Columns of
        false => (
          TextIO.output( Out, 
            String.concat( 
              List.map 
                ( fn X => toString X ^ Delim )  
                ( List.take( Xs, ( List.length Xs )-1 ) ) ) );
          TextIO.output( Out, toString( List.last Xs ) ) )
      | true => (
          TextIO.output( Out, 
            String.concat( 
              List.map 
                ( fn X => toString X ^ Delim )  
                ( List.take( Xs, Columns ) ) ) );
          TextIO.output( Out, "\n" );
          write( List.drop( Xs, Columns ) ) )

    val _ = write Xs

    val _ = TextIO.closeOut Out

  in
    ()
  end

  fun readCSV ( fromString : string -> 'a ) 
              ( Filename : string ) 
      : 'a list = 
    readDSV ( fn C => C= #"," ) fromString Filename

  val readCSInts : string -> int list = 
    readCSV ( Option.valOf o Int.fromString )
  val readCSReals : string -> real list = 
    readCSV ( Option.valOf o Real.fromString )

  fun writeCSV( toString : 'a -> string ) 
              ( Xs : 'a list, Filename : string ) : unit =
    writeDSV ( ",", 20 ) toString ( Xs, Filename )

  val writeCSReals : real list * string -> unit = writeCSV Real.toString

  fun readFilenames( File : string ) : string list =
    readDSV 
      ( fn C => C= #"\n" ) 
      ( fn S => 
          String.implode( 
            List.filter ( not o Char.isSpace ) ( String.explode S ) ) )
      File

  fun writeFilenames( Filenames : string list, File : string ) : unit =
    writeDSV ( "", 1 ) ( fn S => S ) ( Filenames, File )


end (* structure TextFileUtil *)
