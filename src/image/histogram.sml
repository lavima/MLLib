(*
* file: histogram.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a polymorphic signature and structure for finding the 
* histogram of an intensity image. 
*)

signature HISTOGRAM =
sig
  
  type image

  val histogram' : 'a image * int -> int Array.array
  val histogram : 'a image -> int Array.array

end (* signature HISTOGRAM *)

signature HISTOGRAM_IMAGE = 
sig

  type image 

  val app : ( 'a -> unit ) -> image -> unit
  val fromReal : real -> 'a
  val less : 'a * 'a -> bool

end (* signature HISTOGRAM_IMAGE *)

functor HistogramFun( ImageSpec : HISTOGRAM_IMAGE ) : HISTOGRAM =
struct

  type image = ImageSpec.image 

  fun histogram' ( numBins : int ) ( im : image ) 
      : int Array.array list = 
  let

    val f = 1.0/( ( real numBins )-1.0 )

    val delims = 
      List.tabulate( 
        numBins+1, 
        fn x => ImageSpec.fromReal( ( ( real x )-0.5 )*f ) )
 
    (*
    * Get the index of the histogram bin to place an element. 
    *)
    fun getIndex( delims : 'a list, element : 'a, index : int )
        : int =
      case delims of 
        low::( delims' as high::_ ) =>
          if not ( ImageSpec.less( element, low ) ) andalso
             ImageSpec.less( element, high ) then
            index
          else
            getIndex( delims', element, index+1 )
      | _ => raise Overflow
          
    val histogram = Array.array( numBins, 0 )
    val _ = ImageSpec.app 
      ( fn( element ) => 
        let
          val index = getIndex( delims, element, 0 )
          val count = Array.sub( histogram, index )
        in
          Array.update( histogram, index, count+1 ) 
        end )
      im

  in
    histogram 
  end

  val histogram = histogram' 256

end (* functor HistogramFun *)
