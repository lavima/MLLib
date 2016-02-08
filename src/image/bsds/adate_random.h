/*
* file: adate_random.h
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains the function declarations for the random number generator
* in ADATE.
*/

#ifndef ADATE_RANDOM
#define ADATE_RANDOM

extern "C" {

void aSeed( uint64_t Seed );
double aRand();

}

#endif
