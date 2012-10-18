/************************************************************
*																			 	*
*	AUTHORS : CLAUDIO AGOSTINELLI and ALESSANDRO GAGLIARDI	*
*	AIM : COMPUTE THE MEAN CIRCULAR									*
*	DATA : 18 OCTOBER 2012.												*
*																				*
*************************************************************/

#include <R_ext/Arith.h> //To have NA variable type.
#include <R_ext/Constants.h>//To have this constant .Machine$double.eps that is labeled "DOUBLE_EPS".

void MeanCircularRad(double*,int*,double*); //prototype of function

void MeanCircularRad(double *x,int *n,double *result)
{
	double sinr = 0.0;
	double cosr = 0.0;
	double circmean = NA_REAL;
	int i;

	for(i=0;i<(*n);i++)
	{
       sinr += sin(x[i]);
       cosr += cos(x[i]);
	}
   if (sqrt(pow(sinr,2) + pow(cosr,2))/(*n) > DOUBLE_EPS)
   	circmean = atan2(sinr, cosr);

	*result = circmean;
}

