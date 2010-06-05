coordinates2radiants	<- function(coordsdata) {

	numberofcoords	<- nrow(coordsdata)

	radiants		<- data.frame(matrix(0,numberofcoords,1))

	for (l in 1:numberofcoords) {

	Xcor	<- coordsdata[l,1]
	Ycor	<- coordsdata[l,2]

		if(Xcor == 0 & Ycor == 0) { 
			radiants[l,1]	<- NULL 
		}

		if(Xcor > 0 & Ycor == 0) { 
			radiants[l,1]	<- 0
		}

		if(Xcor < 0 & Ycor == 0) { 
			radiants[l,1]	<- pi
		}

		if(Xcor == 0 & Ycor > 0) { 
			radiants[l,1]	<- pi / 2
		}

		if(Xcor == 0 & Ycor < 0) { 
			radiants[l,1]	<- 3 * pi / 2
		}

		if(Xcor > 0 & Ycor > 0) { 
			radiants[l,1]	<- atan(Ycor / Xcor) 
		}

		if(Xcor > 0 & Ycor < 0) { 
			Ycortemp	<- Ycor * (-1)
			radiants[l,1]	<- 2 * pi - atan(Ycortemp / Xcor) 
		}

		if(Xcor < 0 & Ycor > 0) {
			Xcortemp	<- Xcor * (-1) 
			radiants[l,1]	<- pi - atan(Ycor / Xcortemp) 
		}

		if(Xcor < 0 & Ycor < 0) {
			Ycortemp	<- Ycor * (-1)
			Xcortemp	<- Xcor * (-1) 
			radiants[l,1]	<- pi + atan(Ycortemp / Xcortemp) 
		} 

	} # for END

	return(radiants)

}