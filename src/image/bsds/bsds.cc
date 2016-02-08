/*
* filename: bsds.cc
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file provides a C interface the utility functions in the BSDS benchmark 
* platform. 
*/

#include <stdint.h>

#include "Matrix.hh"
#include "match.hh"

extern "C" double bsdsMatchEdges(double *bitmapData1, double *bitmapData2, 
                                 int32_t width, int32_t height,
                                 double maxDist, double outlierCost, 
                                 double *matchData1, double *matchData2) {

  Matrix bitmap1(height, width, bitmapData1); 
  Matrix bitmap2(height, width, bitmapData2); 

  Matrix match1(height, width, matchData1); 
  Matrix match2(height, width, matchData2); 

  return matchEdgeMaps(bitmap1, bitmap2, maxDist, outlierCost, match1, match2);
}
