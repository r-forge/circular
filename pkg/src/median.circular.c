
/****************************************************************
*                                                               *
*       AUTHORS : CLAUDIO AGOSTINELLI and ALESSANDRO GAGLIARDI  *
*       AIM : COMPUTE THE MEDIAN CIRCULAR                       *
*       DATA : 10 NOVEMBER 2012.                                *
*                                                               *
*****************************************************************/


#include <R.h>
#include <math.h>
#include <stdlib.h>
#include <memory.h>
#include "mean.circular.h"
#include "median.circular.h"

/*
*	This function compute the circular median and return, the median value and all candidate observations for median values
*	To use this function witohut all candidate observations for median values, write as follow :
*
*			int a = 0;
*			MedianCircularRad(x,n,result,(double*)NULL,&a);
*/

void MedianCircularRad(double *x,int *n,double *result,double *medians,int *lMedians)
{
	double valueOfDev;
	int i,k=0;
	double minimumObs[(*n)];
	double minimum = PI;
	for(i=0;i<(*n);i++)
	{
		valueOfDev = dev(x,x[i],n);
		if(valueOfDev - minimum < -DOUBLE_EPS)
		{
			minimum = valueOfDev;
			minimumObs[0] = x[i];
			k=1;
		}
		else if(fabs(valueOfDev-minimum)/(*n)<=DOUBLE_EPS)
		{
			minimumObs[k++] = x[i];
		}
	}
	MeanCircularRad(minimumObs,&k,result);
	if((*lMedians)>0)
	{
		memcpy(medians,minimumObs,sizeof(double)*k);
		*lMedians = k;
	}

}

double dev(double *theta,double xv,int *n)
{
	double values=0;
	int j;
	for(j=0;j<(*n);j++)
	{
		values += fabs(PI-fabs(theta[j]-xv));
	}
	values /= (*n);
	values = PI - values;
	return(values);
}

