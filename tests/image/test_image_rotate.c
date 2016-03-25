#include <stdio.h>
#include <math.h>
//#include "../../ffi.h"
/*
 * Compute rotated 2D matrix using bilinear interpolation.
 */

/*

void fitest_image_rotate(
   Pointer            src,
   Pointer            dst,
   uint64_t size_x_src,  
   uint64_t size_y_src,
   uint64_t size_x_dst,  
   uint64_t size_y_dst,
   double ori)           
{
   double* m_src = (double*)src;
   double* m_dst = (double*)dst;
   unsigned long size_x_src = (unsigned long)size_x_src;
   unsigned long size_y_src = (unsigned long)size_y_src;
   unsigned long size_x_dst = (unsigned long)size_x_dst;
   unsigned long size_y_dst = (unsigned long)size_y_dst;

*/

void fitest_image_rotate(
   double*            src,  /* source matrix */
   double*            dst,  /* destination matrix */
   unsigned long size_x_src,  /* size of source */
   unsigned long size_y_src,
   unsigned long size_x_dst,  /* size of destination */
   unsigned long size_y_dst,
   double ori)                /* orientation */
{
   double* m_src = (double*)src;
   double* m_dst = (double*)dst;

   /* check that matrices are nonempty */
   if ((size_x_src > 0) && (size_y_src > 0) &&
       (size_x_dst > 0) && (size_y_dst > 0))
   {
      /* compute sin and cos of rotation angle */
      const double cos_ori = cos(ori);
      const double sin_ori = sin(ori);
      /* compute location of origin in src */
      const double origin_x_src = (double)((size_x_src - 1)) / 2;
      const double origin_y_src = (double)((size_y_src - 1)) / 2;
      /* rotate */
      double u = -((double)((size_x_dst - 1)) / 2);
      unsigned long n = 0;
      for (unsigned long dst_x = 0; dst_x < size_x_dst; dst_x++) {
         double v = -((double)((size_y_dst - 1)) / 2);
         for (unsigned long dst_y = 0; dst_y < size_y_dst; dst_y++) {
            /* reverse rotate by orientation and shift by origin offset */
            double x = u * cos_ori + v * sin_ori + origin_x_src;
            double y = v * cos_ori - u * sin_ori + origin_y_src;
            /* check that location is in first quadrant */
            if ((x >= 0) && (y >= 0)) {
               /* compute integer bounds on location */
               unsigned long x0 = (unsigned long)(floor(x));
               unsigned long x1 = (unsigned long)(ceil(x));
               unsigned long y0 = (unsigned long)(floor(y));
               unsigned long y1 = (unsigned long)(ceil(y));
               /* check that location is within src matrix */
               if ((0 <= x0) && (x1 < size_x_src) &&
                   (0 <= y0) && (y1 < size_y_src))
               {
                  /* compute distances to bounds */
                  double dist_x0 = x - x0;
                  double dist_x1 = x1 - x;
                  double dist_y0 = y - y0;
                  double dist_y1 = y1 - y;
                  /* grab matrix elements */
                  const double m00 = m_src[x0*size_y_src + y0];
                  const double m01 = m_src[x0*size_y_src + y1];
                  const double m10 = m_src[x1*size_y_src + y0];
                  const double m11 = m_src[x1*size_y_src + y1];

                  /* interpolate in x-direction */
                  const double t0 =
                     (x0 != x1) ? (dist_x1 * m00 + dist_x0 * m10) : m00;
                  const double t1 =
                     (x0 != x1) ? (dist_x1 * m01 + dist_x0 * m11) : m01;
                  /* interpolate in y-direction */

                  m_dst[n] = (y0 != y1) ? (dist_y1 * t0 + dist_y0 * t1) : t0;
               }
            }
            /* increment coordinate */
            n++;
            v++;
         }
         u++;
      }
   }
}


