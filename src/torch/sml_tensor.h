/*
* file: sml_tensor.h
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains the function declarations needed for the SML integration
*/

#pragma once

Pointer SMLTensorNew(Pointer dims, int32_t value);
Pointer SMLTensorNew(Pointer dims, float value);
Pointer SMLTensorNew(Pointer dims, int32_t dtype, Pointer *values);
