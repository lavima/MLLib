/*
* filename: f_measure.c
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file provides an SML interface to the matchEdgeMaps function in the BSDS
* benchmark platform. 
*/

#include <stdint.h>

#include "../ffi.h"
#include "bsds/bsds.h"

double fiMatchEdges(Pointer bitmap1, Pointer bitmap2, 
                    int32_t width, int32_t height,
                    double maxDist, double outlierCost, 
                    Pointer match1, Pointer match2) {
  return bsdsMatchEdges((double *)bitmap1, (double *)bitmap2, 
          width, height, maxDist, outlierCost, 
          (double *)match1, (double *)match2);
}
