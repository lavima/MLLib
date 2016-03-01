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

end (* structure PNM *)
