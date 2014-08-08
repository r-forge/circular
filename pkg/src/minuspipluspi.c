/****************************************************************
*                                                               *
*       AUTHORS : CLAUDIO AGOSTINELLI and ALESSANDRO GAGLIARDI  *
*       AIM : MinusPiPlusPiRad from internal function 	        *
*					in circular package     *
*       DATA : 02 NOVEMBER 2012.                                *
*                                                               *
*****************************************************************/

#include <R.h>
#include <math.h>
#include "minuspipluspi.h"

void MinusPiPlusPiRadOLD(double *x,int *n) {
  int i;
  for (i=0;i<(*n);i++) {
    if (x[i] < -PI) {
      x[i] = x[i] + (2 * PI);
    }
    if (x[i] > PI) {
      x[i] = x[i] - (2 * PI);
    }
  }
}

void MinusPiPlusPiRad(double *x,int *n) {
  int i;
  for (i=0;i<(*n);i++) {
     x[i] = (x[i] < -PI)?(x[i] + (2 * PI)):(x[i]);
     x[i] = (x[i] > PI)?(x[i] - (2 * PI)):(x[i]);
  }
}
