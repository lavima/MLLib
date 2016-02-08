(* 
* filename: pnm_common.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with common functionality for the PNM format
*)


structure PNMCommon =
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

  fun getDepth( Format : format ) : int =
    case Format of
      plainPBM => 1
    | plainPGM => 1
    | plainPPM => 3
    | rawPBM => 1
    | rawPGM => 1
    | rawPPM => 3
    | rawPAM Depth => Depth

end (* structure PNMCommon *)
