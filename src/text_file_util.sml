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
              ( filename : string )
      : 'a list =
  let

    val input = TextIO.openIn filename

    fun read() : 'a list = 
      case TextIO.inputLine input of 
        NONE => []
      | SOME line => 
        let
          val xs = String.tokens isDelim line
        in
          ( List.map fromString xs ) @ read()
        end

    val xs = read()

    val _ = TextIO.closeIn input
  in
    xs
  end

  fun readDSV' ( isDelim : char -> bool ) 
              ( fromString : string -> 'a ) 
              ( filename : string )
      : 'a list list =
  let

    val input = TextIO.openIn filename

    fun read() : 'a list list = 
      case TextIO.inputLine input of 
        NONE => []
      | SOME line => 
        let
          val xs = String.tokens isDelim line
        in
          ( List.map fromString xs ) :: read()
        end

    val xs = read()

    val _ = TextIO.closeIn input
  in
    xs
  end
  
  fun writeDSV ( delim : string, columns : int )
               ( toString : 'a -> string ) 
               ( xs : 'a list, filename : string ) : unit =
  let
    val output = TextIO.openOut filename

    fun write( xs : 'a list ) : unit =
      case ( List.length xs )>columns of
        false => (
          TextIO.output( output, 
            String.concat( 
              List.map 
                ( fn x => toString x ^ delim )  
                ( List.take( xs, ( List.length xs )-1 ) ) ) );
          TextIO.output( output, toString( List.last xs ) ) )
      | true => (
          TextIO.output( output, 
            String.concat( 
              List.map 
                ( fn x => toString x ^ delim )  
                ( List.take( xs, columns ) ) ) );
          TextIO.output( output, "\n" );
          write( List.drop( xs, columns ) ) )

    val _ = write xs

    val _ = TextIO.closeOut output

  in
    ()
  end

  fun readCSV ( fromString : string -> 'a ) 
              ( filename : string ) 
      : 'a list = 
    readDSV ( fn c => c= #"," ) fromString filename

  val readCSInts : string -> int list = 
    readCSV ( Option.valOf o Int.fromString )
  val readCSReals : string -> real list = 
    readCSV ( Option.valOf o Real.fromString )

  fun writeCSV( toString : 'a -> string ) 
              ( xs : 'a list, filename : string ) : unit =
    writeDSV ( ",", 20 ) toString ( xs, filename )

  val writeCSReals : real list * string -> unit = writeCSV Real.toString

  fun readFilenames( file : string ) : string list =
    readDSV 
      ( fn c => c= #"\n" ) 
      ( fn s => 
          String.implode( 
            List.filter ( not o Char.isSpace ) ( String.explode s ) ) )
      file

  fun writeFilenames( filenames : string list, file : string ) : unit =
    writeDSV ( "", 1 ) ( fn s => s ) ( filenames, file )

end (* structure TextFileUtil *)
