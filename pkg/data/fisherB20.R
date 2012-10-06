
fisherB20 <- (data.frame(x=cbind(c(67,66,74,61,58,60,100,89,171,166,98,60,197,98,86,123,165,133,101,105,71,84,75,98,83,71,74,91,38,200,56),
												c(107,46,33,67,122,69,43,30,12,25,37,69,5,83,68,36,21,1,71,60,71,71,57,53,38,70,7,48,7,21,27))))
names(fisherB20) <- c("theta","x")

#require(circular)
#fisherB20c <- fisherB20
#fisherB20c$theta <- circular(fisherB20$theta,units="degrees",zero=pi/2,rotation="clock")
#save(fisherB20,file="fisherB20.rda",ascii=TRUE)
#save(fisherB20c,file="fisherB20c.rda",ascii=TRUE)

