
fisherB18 <- (data.frame(x=cbind(c(327,91,88,305,344,270,67,21,281,8,204,86,333,18,57,6,11,27,84),
												c(28,85.2,80.5,4.7,45.9,12.7,72.5,56.6,31.5,112,20,72.5,16,45.9,32.6,56.6,52.6,91.8,55.2))))
names(fisherB18) <- c("theta","x")

#require(circular)
#fisherB18c <- fisherB18
#fisherB18c$theta <- circular(fisherB18$theta,units="degrees",type="directions",zero=pi/2,rotation="clock")
#save(fisherB18,file="fisherB18.rda",ascii=TRUE)
#save(fisherB18c,file="fisherB18c.rda",ascii=TRUE)

